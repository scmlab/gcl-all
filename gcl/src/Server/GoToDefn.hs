{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.GoToDefn
  ( collectLocationLinks,
    TargetRanges (..),
    OriginTargetRanges (..),
  )
where

import Control.Monad.RWS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import GCL.Range
  ( MaybeRanged,
    Range,
    maybeRangeOf,
  )
import Prettyprinter
import Server.IntervalMap
import qualified Server.IntervalMap as IntervalMap
import Syntax.Abstract
import Syntax.Common

collectLocationLinks :: Program -> IntervalMap OriginTargetRanges
collectLocationLinks program = runM (programToScopes program) (collect program)

--------------------------------------------------------------------------------

-- | Stage 1: Only target information (where the definition is)
data TargetRanges = TargetRanges
  { targetRange :: Range,
    targetSelectionRange :: Range
  }
  deriving (Show, Eq)

instance Pretty TargetRanges where
  pretty (TargetRanges tgt tgtSel) =
    pretty "TargetRanges" <+> braces (pretty tgt <> pretty ", " <> pretty tgtSel)

-- | Stage 2: Both origin and target information, but no URI yet
data OriginTargetRanges = OriginTargetRanges
  { originSelectionRange :: Range,
    otTargetRange :: Range,
    otTargetSelectionRange :: Range
  }
  deriving (Show, Eq)

instance Pretty OriginTargetRanges where
  pretty (OriginTargetRanges orig tgt tgtSel) =
    pretty "OriginTargetRanges" <+> braces (pretty orig <+> pretty "->" <+> pretty tgt <> pretty ", " <> pretty tgtSel)

-- | Extracts Scopes from a Program
-- | The TargetRanges here contains only the "target" info, i.e. the definition / declaration parts.
-- | It will become OriginTargetRanges when we have the "origin" info during collection.
programToScopes :: Program -> [Scope TargetRanges]
programToScopes (Program defns decls _ _ _) = [topLevelScope] -- we only have a single scope for now
  where
    topLevelScope :: Map Text.Text TargetRanges
    topLevelScope = Map.mapKeys nameToText targetRanges

    targetRanges :: Map Name TargetRanges
    targetRanges = targetRangesFromDecls <> targetRangesFromDefns

    targetRangesFromDecls :: Map Name TargetRanges
    targetRangesFromDecls =
      makeTargetRanges $ Map.fromList $ concatMap splitDecl decls

    targetRangesFromDefns :: Map Name TargetRanges
    targetRangesFromDefns =
      makeTargetRanges $ Map.fromList $ concatMap splitDefn defns

    -- locationLinksFromFuncDefns :: Map Name LocationLinkToBe
    -- locationLinksFromFuncDefns = makeLocationLinks funcDefns

    -- locationLinksFromTypeDefns :: Map Name LocationLinkToBe
    -- locationLinksFromTypeDefns = makeLocationLinks typeDefns

    -- split a parallel declaration into many simpler declarations
    splitDecl :: Declaration -> [(Name, Declaration)]
    splitDecl decl@(ConstDecl names _ _ _) = [(name, decl) | name <- names]
    splitDecl decl@(VarDecl names _ _ _) = [(name, decl) | name <- names]

    splitDefn :: Definition -> [(Name, Definition)]
    splitDefn def@(TypeDefn con _params ctors _) = (con, def) : concatMap (`splitCtor` def) ctors
    splitDefn FuncDefnSig {} = mempty
    splitDefn def@(FuncDefn name _exprs) = [(name, def)]

    splitCtor :: TypeDefnCtor -> Definition -> [(Name, Definition)]
    splitCtor (TypeDefnCtor name _params) def = [(name, def)]

--  Helper function for converting
--      a Map of "names" and "targets"
--   to a Map of "names" and "TargetRanges"
--
--  For example:
--
--    ╔═════ where the user clicks ════╗
--    ║                                ║
--    ║             double 3           ║
--    ║  origin ──▶ ~~~~~~             ║
--    ║                                ║
--    ╚════════════════════════════════╝
--
--    ╔═══════ where it leads to ══════╗
--    ║                                ║
--    ║             double x = x * 2   ║
--    ║    name ──▶ ~~~~~~             ║
--    ║  target ──▶ ~~~~~~~~~~~~~~~~   ║
--    ║                                ║
--    ╚════════════════════════════════╝

makeTargetRanges :: (MaybeRanged a) => Map Name a -> Map Name TargetRanges
makeTargetRanges = Map.mapMaybeWithKey $ \name target -> do
  tgtRange <- maybeRangeOf target
  tgtSelectionRange <- maybeRangeOf name
  return $
    TargetRanges
      { targetRange = tgtRange,
        targetSelectionRange = tgtSelectionRange
      }

scopeFromLocalBinders :: [Name] -> Scope TargetRanges
scopeFromLocalBinders names =
  Map.mapKeys nameToText $ makeTargetRanges $ Map.fromList $ zip names names

--------------------------------------------------------------------------------
-- Names

instance Collect TargetRanges OriginTargetRanges Name where
  collect name = do
    result <- lookupScopes (nameToText name)
    case result of
      Nothing -> return ()
      Just targetRanges -> case maybeRangeOf name of
        Nothing -> return ()
        Just originRange ->
          let originTargetRanges =
                OriginTargetRanges
                  { originSelectionRange = originRange,
                    otTargetRange = targetRange targetRanges,
                    otTargetSelectionRange = targetSelectionRange targetRanges
                  }
           in tell $ IntervalMap.singleton originRange originTargetRanges

--------------------------------------------------------------------------------
-- Program

instance Collect TargetRanges OriginTargetRanges Program where
  collect (Program defns decls _ stmts _) = do
    collect defns
    collect decls
    collect stmts

--------------------------------------------------------------------------------
-- Definition
instance Collect TargetRanges OriginTargetRanges Definition where
  collect TypeDefn {} = return ()
  collect (FuncDefnSig n t prop _) = do
    collect n
    collect t
    collect prop
  collect (FuncDefn n exprs) = do
    collect n
    collect exprs

--------------------------------------------------------------------------------
-- Declaration

instance Collect TargetRanges OriginTargetRanges Declaration where
  collect = \case
    ConstDecl a _ c _ -> do
      collect a
      collect c
    VarDecl a _ c _ -> do
      collect a
      collect c

--------------------------------------------------------------------------------
-- Stmt

instance Collect TargetRanges OriginTargetRanges Stmt where
  collect = \case
    Assign a b _ -> do
      collect a
      collect b
    Assert a _ -> collect a
    LoopInvariant a b _ -> do
      collect a
      collect b
    Do a _ -> collect a
    If a _ -> collect a
    _ -> return ()

instance Collect TargetRanges OriginTargetRanges GdCmd where
  collect (GdCmd gd stmts _) = do
    collect gd
    collect stmts

--------------------------------------------------------------------------------

instance Collect TargetRanges OriginTargetRanges Expr where
  collect = \case
    Lit _ _ -> return ()
    Var a _ -> collect a
    Const a _ -> collect a
    Op op -> collect op
    Chain ch -> collect ch
    App a b _ -> collect a >> collect b
    Lam _ b _ -> collect b
    Tuple as -> mapM_ collect as
    Quant op args c d _ -> do
      collect op
      localScope (scopeFromLocalBinders args) $ do
        collect c
        collect d
    -- RedexKernel/RedexShell will only appear in proof obligations, not in code
    RedexKernel {} -> return ()
    RedexShell {} -> return ()
    ArrIdx e i _ -> do
      collect e
      collect i
    ArrUpd e i f _ -> do
      collect e
      collect i
      collect f
    -- TODO: provide types for tokens in patterns
    Case e _ _ -> do
      collect e

-- collect patterns

-- instance Collect CaseClause where
--   collect (CaseClause ctor args body) = do
--     collect ctor
--     localScope args $ do
--       collect body

instance Collect TargetRanges OriginTargetRanges ArithOp where
  collect _ = return ()

instance Collect TargetRanges OriginTargetRanges ChainOp where
  collect _ = return ()

instance Collect TargetRanges OriginTargetRanges Chain where
  collect (Pure expr _) = collect expr
  collect (More ch' op expr _) = collect ch' >> collect op >> collect expr

instance Collect TargetRanges OriginTargetRanges FuncClause where
  collect _ = return ()

instance Collect TargetRanges OriginTargetRanges QuantOp' where
  collect (Left op) = collect op
  collect (Right expr) = collect expr

--------------------------------------------------------------------------------

-- | Types
instance Collect TargetRanges OriginTargetRanges Type where
  collect = \case
    TBase _ _ -> return ()
    TArray i x _ -> collect i >> collect x
    TTuple _ -> return ()
    TFunc l r _ -> collect l >> collect r
    TOp _ -> return ()
    TData n _ -> collect n
    TApp x y _ -> collect x >> collect y
    TVar _ _ -> return ()
    TMetaVar _ _ -> return ()
    TType -> return ()

instance Collect TargetRanges OriginTargetRanges Interval where
  collect (Interval x y _) = collect x >> collect y

instance Collect TargetRanges OriginTargetRanges Endpoint where
  collect = \case
    Including x -> collect x
    Excluding x -> collect x
