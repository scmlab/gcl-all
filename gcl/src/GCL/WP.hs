{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module GCL.WP (WP, TM, sweep, structStmts, runWP, collectTypedHole) where

import Control.Monad.Except
  ( MonadError (throwError),
    runExcept,
  )
import Control.Monad.RWS (RWST (runRWST))
import Data.IntMap (IntMap)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import GCL.Predicate
  ( Hole (Hole),
    InfMode (..),
    PO (..),
    Pred,
    Spec (..),
  )
import GCL.Range (MaybeRanged (..))
import GCL.WP.SP
import GCL.WP.Struct
import GCL.WP.Types
import GCL.WP.WP
import Syntax.Common.Types (nameToText)
import Syntax.Typed
import Syntax.Typed.Operator (true)
import Syntax.Typed.Util
  ( declaredNames,
    programToScopeForSubstitution,
  )

runWP ::
  WP a ->
  (Decls, [[Text]]) ->
  Int ->
  Either
    StructError
    (a, Int, ([PO], [Spec], [StructWarning], IntMap (Int, Expr)))
runWP p decls counter = runExcept $ runRWST p decls counter

sweep ::
  Program ->
  Either StructError ([PO], [Spec], [Hole], [StructWarning], IntMap (Int, Expr), Int)
sweep program@(Program _ decs _props stmts _) = do
  let decls = programToScopeForSubstitution program
  let dnames = [map nameToText $ declaredNames decs]
  (_, counter, (pos, specs, warnings, redexes)) <-
    runWP (structProgram stmts) (decls, dnames) 0
  -- collect holes from expressions
  let holes = collectTypedHole program
  -- update Proof Obligations with corresponding Proof Anchors
  let proofAnchors =
        stmts >>= \case
          Proof anchor _ r -> [(anchor, r)]
          _ -> []
  -- make a table of (#hash, range) from Proof Anchors
  let table = Map.fromList proofAnchors
  let updatePO po = case Map.lookup (poAnchorHash po) table of
        Nothing -> po
        Just range -> po {poAnchorRange = Just range}

  let pos' = map updatePO pos

  return (pos', specs, holes, warnings, redexes, counter)

--------------------------------------------------------------------------------

data ProgView
  = ProgViewEmpty
  | ProgViewOkay Pred [Stmt] Pred
  | ProgViewMissingPrecondition [Stmt] Pred
  | ProgViewMissingPostcondition Pred [Stmt]
  | ProgViewMissingBoth [Stmt]

progView :: [Stmt] -> ProgView
progView [] = ProgViewEmpty
progView [Assert pre _] = do
  ProgViewMissingPrecondition [] pre
progView stmts = do
  case (head stmts, last stmts) of
    (Assert pre _, Assert post _) -> do
      ProgViewOkay pre (init (tail stmts)) post
    (Assert pre _, _) -> do
      ProgViewMissingPostcondition pre (tail stmts)
    (_, Assert post _) -> do
      ProgViewMissingPrecondition (init stmts) post
    _ -> ProgViewMissingBoth stmts

structProgram :: [Stmt] -> WP ()
structProgram stmts = do
  case progView (removeLastProofs stmts) of
    ProgViewEmpty -> return ()
    ProgViewOkay pre stmts' post ->
      structStmts Primary (pre, Nothing) stmts' post
    ProgViewMissingPrecondition stmts' post ->
      structStmts Primary (true, Nothing) stmts' post
    ProgViewMissingPostcondition _ stmts' ->
      throwError . MissingPostcondition . maybeRangeOf . last $ stmts'
    ProgViewMissingBoth stmts' ->
      throwError . MissingPostcondition . maybeRangeOf . last $ stmts'
  where
    -- ignore Proofs after the Postcondition
    removeLastProofs :: [Stmt] -> [Stmt]
    removeLastProofs = List.dropWhileEnd isProof

    isProof :: Stmt -> Bool
    isProof Proof {} = True
    isProof _ = False

-- tying the knots
-- handling mutual recursion functions across modules

structStmts :: InfMode -> (Pred, Maybe Expr) -> [Stmt] -> Pred -> WP ()
structStmts = this
  where
    (this, structSegs, struct) =
      structFunctions
        ( wpSegs,
          wpSStmts,
          wp,
          spSStmts
        )
    (wpSegs, wpSStmts, wp) = wpFunctions structSegs
    spSStmts = spFunctions (structSegs, struct)

-- Hole collection

class CollectTypedHole a where
  collectTypedHole :: a -> [Hole]

instance (CollectTypedHole a) => CollectTypedHole [a] where
  collectTypedHole = concatMap collectTypedHole

instance (CollectTypedHole a) => CollectTypedHole (Maybe a) where
  collectTypedHole (Just a) = collectTypedHole a
  collectTypedHole Nothing = mempty

instance CollectTypedHole Program where
  -- Deduplicates the list of holes since declaration might gives duplication result.
  collectTypedHole (Program defs decls exprs stmts _) =
    List.nub $
      collectTypedHole defs <> collectTypedHole decls <> collectTypedHole exprs <> collectTypedHole stmts

instance CollectTypedHole Definition where
  collectTypedHole (TypeDefn {}) = mempty
  collectTypedHole (ValDefn _ _ expr) = collectTypedHole expr

instance CollectTypedHole Declaration where
  collectTypedHole (ConstDecl _ _ expr _) = collectTypedHole expr
  collectTypedHole (VarDecl _ _ expr _) = collectTypedHole expr

instance CollectTypedHole Stmt where
  collectTypedHole (Skip {}) = mempty
  collectTypedHole (Abort {}) = mempty
  collectTypedHole (Assign _ exprs _) = collectTypedHole exprs
  collectTypedHole (AAssign a b c _) = collectTypedHole a <> collectTypedHole b <> collectTypedHole c
  collectTypedHole (Assert expr _) = collectTypedHole expr
  collectTypedHole (LoopInvariant a b _) = collectTypedHole a <> collectTypedHole b
  collectTypedHole (Do guards _) = collectTypedHole guards
  collectTypedHole (If guards _) = collectTypedHole guards
  collectTypedHole (Spec {}) = mempty
  collectTypedHole (Proof {}) = mempty
  collectTypedHole (Alloc _ exprs _) = collectTypedHole exprs
  collectTypedHole (HLookup _ expr _) = collectTypedHole expr
  collectTypedHole (HMutate _ expr _) = collectTypedHole expr
  collectTypedHole (Dispose expr _) = collectTypedHole expr
  collectTypedHole (Block program _) = collectTypedHole program

instance CollectTypedHole GdCmd where
  collectTypedHole (GdCmd guard body _) = collectTypedHole guard <> collectTypedHole body

instance CollectTypedHole Expr where
  collectTypedHole (Lit {}) = mempty
  collectTypedHole (Var {}) = mempty
  collectTypedHole (Const {}) = mempty
  collectTypedHole (Op {}) = mempty
  collectTypedHole (Chain chain) = collectTypedHole chain
  collectTypedHole (App a b _) = collectTypedHole a <> collectTypedHole b
  collectTypedHole (Lam _ _ expr _) = collectTypedHole expr
  collectTypedHole (Tuple exprs) = collectTypedHole exprs
  collectTypedHole (OutT _ expr) = collectTypedHole expr
  collectTypedHole (Quant a _ b c _) = collectTypedHole a <> collectTypedHole b <> collectTypedHole c
  collectTypedHole (ArrIdx a b _) = collectTypedHole a <> collectTypedHole b
  collectTypedHole (ArrUpd a b c _) = collectTypedHole a <> collectTypedHole b <> collectTypedHole c
  collectTypedHole (Case expr clauses _) = collectTypedHole expr <> collectTypedHole clauses
  collectTypedHole (Subst expr substs) = collectTypedHole expr <> (collectTypedHole . map snd) substs
  collectTypedHole (EHole _ i ty range env) = [Hole i ty range env]

instance CollectTypedHole CaseClause where
  collectTypedHole (CaseClause _ expr) = collectTypedHole expr

instance CollectTypedHole Chain where
  collectTypedHole (Pure expr) = collectTypedHole expr
  collectTypedHole (More c _ _ expr) = collectTypedHole c <> collectTypedHole expr
