{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Load2 where

import qualified Data.IntMap as IntMap
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Predicate (Hole, PO, Spec)
import GCL.Range (Range, posCol, posLine, rangeEnd, rangeStart)
import GCL.WP.Types (StructWarning)
import qualified GCL.Type as TypeChecking
import qualified GCL.WP as WP
import Server.GoToDefn (OriginTargetRanges, collectLocationLinks)
import Server.Highlighting (collectHighlighting)
import Server.Hover (collectHoverInfo)
import Server.IntervalMap (IntervalMap)
import Server.Monad (HoleKind (..))
import qualified Language.LSP.Protocol.Types as LSP
import qualified Syntax.Concrete as C
import qualified Syntax.Concrete.Instances.ToAbstract as C
import Syntax.Concrete.Types (GdCmd (..), SepBy (..))
import qualified Syntax.Parser as Parser

--------------------------------------------------------------------------------
-- Types

-- | Result of digging holes: (edits in original coordinates, new source after edits)
type DigResult = ([(Range, Text)], Text)

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
-- Main pipeline

-- | Full pipeline: parseAndDig → loadConcrete.
-- Returns Left for errors, Right (Nothing, result) if no holes were found,
-- Right (Just digResult, result) if holes were dug.
loadAndDig :: FilePath -> Text -> Either Error (Maybe DigResult, LoadResult)
loadAndDig filePath source = do
  (maybeDig, concrete) <- parseAndDig filePath source
  result <- loadConcrete concrete
  return (maybeDig, result)

-- | Pipeline from concrete AST: abstract → elaborate → sweep.
loadConcrete :: C.Program -> Either Error LoadResult
loadConcrete concrete = do
  let abstract = C.runAbstractTransform concrete
  elaborated <- case TypeChecking.runElaboration abstract mempty of
    Left err -> Left (TypeError err)
    Right e  -> Right e
  (pos, specs, holes, warnings, _redexes, idCount) <- case WP.sweep elaborated of
    Left err -> Left (StructError err)
    Right r  -> Right r
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

-- | Parse source, and if holes are found, dig them and re-parse.
-- Returns (if holes were dug) the DigResult, and the clean concrete AST.
parseAndDig :: FilePath -> Text -> Either Error (Maybe DigResult, C.Program)
parseAndDig filePath source = do
  (concrete1, holes1) <- parseAndCollectHoles filePath source
  case holes1 of
    [] -> Right (Nothing, concrete1)
    _  -> do
      let (edits, newSource) = replaceHoles source holes1
      (concrete2, holes2) <- parseAndCollectHoles filePath newSource
      case holes2 of
        [] -> Right (Just (edits, newSource), concrete2)
        _  -> Left (Others "Load2" "unexpected holes after digging" Nothing)
  where
    parseAndCollectHoles fp src =
      case Parser.scanAndParse Parser.program fp src of
        Left err       -> Left (ParseError err)
        Right concrete -> Right (concrete, collectHoles concrete)
    replaceHoles src holeList = (edits, applyEdits src edits)
      where
        edits = map (\(kind, range) -> (range, diggedText kind range)) holeList

--------------------------------------------------------------------------------
-- Hole digging

-- | Replacement text for a hole, matching the behaviour of Server.Monad.digHoles.
diggedText :: HoleKind -> Range -> Text
diggedText StmtHole range = "[!\n" <> ind <> "\n" <> ind <> "!]"
  where ind = Text.replicate (posCol (rangeStart range) - 1) " "
diggedText ExprHole _ = "{! !}"

-- | Apply a list of non-overlapping edits to source text.
-- Edits must not overlap. They are applied using positions in the original source,
-- processed from first to last.
applyEdits :: Text -> [(Range, Text)] -> Text
applyEdits source edits =
  let ls = Text.split (== '\n') source
      lineStarts = IntMap.fromList $ zip [1 ..] $ scanl (\off l -> off + Text.length l + 1) 0 ls
      toOffset pos = IntMap.findWithDefault 0 (posLine pos) lineStarts + (posCol pos - 1)
      sorted = sortBy (comparing (rangeStart . fst)) edits
      go pos [] = Text.drop pos source
      go pos ((range, replacement) : rest) =
        let s = toOffset (rangeStart range)
            e = toOffset (rangeEnd range)
         in Text.take (s - pos) (Text.drop pos source)
              <> replacement
              <> go e rest
   in go 0 sorted

-- | Same as applyEdits but uses a line-column cursor instead of offset conversion.
-- Avoids the line-ending ambiguity of toOffset.
applyEdits2 :: Text -> [(Range, Text)] -> Text
applyEdits2 source edits =
  Text.concat $ go (1, 1) source (sortBy (comparing (rangeStart . fst)) edits)
  where
    go _ src [] = [src]
    go cur src all@((range, repl) : rest) =
      case Text.uncons src of
        Nothing      -> []
        Just (c, cs)
          | curAt cur (rangeStart range) -> repl : skipTo (rangeEnd range) cur src rest
          | otherwise                    -> Text.singleton c : go (step c cur) cs all

    skipTo endPos cur src rest =
      case Text.uncons src of
        Nothing      -> go cur Text.empty rest
        Just (c, cs)
          | curAt cur endPos -> go cur src rest
          | otherwise        -> skipTo endPos (step c cur) cs rest

    curAt (l, col) pos = l == posLine pos && col == posCol pos

    step '\n' (l, _)   = (l + 1, 1)
    step _    (l, col) = (l, col + 1)

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
