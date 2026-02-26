{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Load2 where

import Data.IntMap (IntMap)
import Data.Text (Text)
import Error (Error (..))
import GCL.Predicate (Hole, PO, Spec)
import GCL.Range (Range)
import GCL.WP.Types (StructWarning)
import qualified GCL.Type as TypeChecking
import qualified GCL.WP as WP
import Syntax.Typed (Expr)
import Server.GoToDefn (OriginTargetRanges, collectLocationLinks)
import Server.Highlighting (collectHighlighting)
import Server.Hover (collectHoverInfo)
import Server.IntervalMap (IntervalMap)
import Server.Monad (HoleKind (..))
import qualified Language.LSP.Protocol.Types as LSP
import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import qualified Syntax.Concrete.Instances.ToAbstract as C
import Syntax.Concrete.Types (GdCmd (..), SepBy (..))
import qualified Syntax.Parser as Parser
import qualified Syntax.Typed as T

--------------------------------------------------------------------------------

data LoadHalt
  = HaltHoles [(HoleKind, Range)]
  | HaltError Error

data LoadResult = LoadResult
  { specifications  :: [Spec]
  , holes           :: [Hole]
  , proofObligations :: [PO]
  , warnings        :: [StructWarning]
  , idCount         :: Int
  , semanticTokens  :: [LSP.SemanticTokenAbsolute]
  , definitionLinks :: IntervalMap OriginTargetRanges
  , hoverInfos      :: IntervalMap LSP.Hover
  }

--------------------------------------------------------------------------------

-- | Pure pipeline: FilePath is only for error reporting, not for reading.
load :: FilePath -> Text -> Either LoadHalt LoadResult
load filePath source = do
  concrete                                           <- parse filePath source
  abstract                                           <- holesOrAbstract concrete
  elaborated                                         <- elaborate abstract
  (pos, specs, holes, warnings, _redexes, idCount)   <- sweep elaborated
  return LoadResult
    { specifications   = specs
    , holes            = holes
    , proofObligations = pos
    , warnings         = warnings
    , idCount          = idCount
    , semanticTokens   = collectHighlighting concrete
    , definitionLinks  = collectLocationLinks abstract
    , hoverInfos       = collectHoverInfo elaborated
    }

--------------------------------------------------------------------------------

parse :: FilePath -> Text -> Either LoadHalt C.Program
parse filePath source =
  case Parser.scanAndParse Parser.program filePath source of
    Left err       -> Left (HaltError (ParseError err))
    Right concrete -> Right concrete

holesOrAbstract :: C.Program -> Either LoadHalt A.Program
holesOrAbstract concrete =
  case collectHoles concrete of
    []    -> Right (C.runAbstractTransform concrete)
    holes -> Left (HaltHoles holes)

elaborate :: A.Program -> Either LoadHalt T.Program
elaborate abstract =
  case TypeChecking.runElaboration abstract mempty of
    Left err       -> Left (HaltError (TypeError err))
    Right elaborated -> Right elaborated

sweep :: T.Program -> Either LoadHalt ([PO], [Spec], [Hole], [StructWarning], IntMap (Int, Expr), Int)
sweep elaborated =
  case WP.sweep elaborated of
    Left err     -> Left (HaltError (StructError err))
    Right result -> Right result

--------------------------------------------------------------------------------
-- Collecting holes from concrete syntax

collectHoles :: C.Program -> [(HoleKind, Range)]
collectHoles (C.Program _ statements) = collectHolesFromStatements statements

collectHolesFromStatements :: [C.Stmt] -> [(HoleKind, Range)]
collectHolesFromStatements = concatMap collectHolesFromStatement

collectHolesFromStatement :: C.Stmt -> [(HoleKind, Range)]
collectHolesFromStatement (C.SpecQM range)              = [(StmtHole, range)]
collectHolesFromStatement (C.Assign _ _ exprs)          = concat $ mapSepBy collectHolesFromExpr exprs
collectHolesFromStatement (C.AAssign _ _ a _ _ b)       = collectHolesFromExpr a ++ collectHolesFromExpr b
collectHolesFromStatement (C.Assert _ a _)              = collectHolesFromExpr a
collectHolesFromStatement (C.LoopInvariant _ a _ _ _ b _) = collectHolesFromExpr a ++ collectHolesFromExpr b
collectHolesFromStatement (C.Do _ ss _)                 = concat $ mapSepBy collectHolesFromGdCmd ss
collectHolesFromStatement (C.If _ ss _)                 = concat $ mapSepBy collectHolesFromGdCmd ss
collectHolesFromStatement (C.Alloc _ _ _ _ es _)        = concat $ mapSepBy collectHolesFromExpr es
collectHolesFromStatement (C.HLookup _ _ _ a)           = collectHolesFromExpr a
collectHolesFromStatement (C.HMutate _ a _ b)           = collectHolesFromExpr a ++ collectHolesFromExpr b
collectHolesFromStatement (C.Dispose _ a)               = collectHolesFromExpr a
collectHolesFromStatement _                             = []

collectHolesFromGdCmd :: C.GdCmd -> [(HoleKind, Range)]
collectHolesFromGdCmd (GdCmd _ _ stmts) = collectHolesFromStatements stmts

collectHolesFromExpr :: C.Expr -> [(HoleKind, Range)]
collectHolesFromExpr (C.HoleQM range)          = [(ExprHole, range)]
collectHolesFromExpr (C.Paren _ expr _)        = collectHolesFromExpr expr
collectHolesFromExpr (C.Arr a _ b _)           = collectHolesFromExpr a ++ collectHolesFromExpr b
collectHolesFromExpr (C.App a b)               = collectHolesFromExpr a ++ collectHolesFromExpr b
collectHolesFromExpr (C.Quant _ _ _ _ a _ b _) = collectHolesFromExpr a ++ collectHolesFromExpr b
collectHolesFromExpr (C.Case _ a _ _)          = collectHolesFromExpr a
collectHolesFromExpr _                         = []

mapSepBy :: (a -> b) -> SepBy s a -> [b]
mapSepBy f (Head c)       = [f c]
mapSepBy f (Delim c _ cs) = f c : mapSepBy f cs
