{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GCL.WP (WP, TM, sweep, structStmts, runWP, collectStmtHoles) where

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
  let holes = collectProgramHoles program
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

collectProgramHoles :: Program -> [Hole]
collectProgramHoles (Program defs decls exprs stmts _) =
  concatMap collectDefHoles defs
    <> concatMap collectDeclHoles decls
    <> concatMap collectExprHoles exprs
    <> concatMap collectStmtHoles stmts
  where
    collectDefHoles :: Definition -> [Hole]
    collectDefHoles (FuncDefnSig _ _ (Just expr) _) = collectExprHoles expr
    collectDefHoles (FuncDefn _ expr) = collectExprHoles expr
    collectDefHoles _ = []

    collectDeclHoles :: Declaration -> [Hole]
    collectDeclHoles (ConstDecl _ _ (Just expr) _) = collectExprHoles expr
    collectDeclHoles (VarDecl _ _ (Just expr) _) = collectExprHoles expr
    collectDeclHoles _ = []

collectStmtHoles :: Stmt -> [Hole]
collectStmtHoles (Assign _ exprs _) = concatMap collectExprHoles exprs
collectStmtHoles (AAssign a b c _) =
  collectExprHoles a
    <> collectExprHoles b
    <> collectExprHoles c
collectStmtHoles (If guards _) = concatMap collectGdCmdHoles guards
collectStmtHoles (Do guards _) = concatMap collectGdCmdHoles guards
collectStmtHoles (Block program _) = collectProgramHoles program
collectStmtHoles (Alloc _ exprs _) = concatMap collectExprHoles exprs
collectStmtHoles (HLookup _ expr _) = collectExprHoles expr
collectStmtHoles (HMutate _ expr _) = collectExprHoles expr
collectStmtHoles (Dispose expr _) = collectExprHoles expr
collectStmtHoles _ = []

collectGdCmdHoles :: GdCmd -> [Hole]
collectGdCmdHoles (GdCmd guard body _) =
  collectExprHoles guard
    <> concatMap collectStmtHoles body

collectExprHoles :: Expr -> [Hole]
collectExprHoles (Chain ch) = collectChainHoles ch
  where
    collectChainHoles :: Chain -> [Hole]
    collectChainHoles (Pure a) =
      collectExprHoles a
    collectChainHoles (More c _ _ a) =
      collectChainHoles c
        <> collectExprHoles a
collectExprHoles (App a b _) =
  collectExprHoles a
    <> collectExprHoles b
collectExprHoles (Lam _ _ a _) =
  collectExprHoles a
collectExprHoles (Quant a _ b c _) =
  collectExprHoles a
    <> collectExprHoles b
    <> collectExprHoles c
collectExprHoles (ArrIdx a b _) =
  collectExprHoles a
    <> collectExprHoles b
collectExprHoles (ArrUpd a b c _) =
  collectExprHoles a
    <> collectExprHoles b
    <> collectExprHoles c
collectExprHoles (Case a _ _) =
  collectExprHoles a
collectExprHoles (Subst a b) =
  collectExprHoles a
    <> concatMap (collectExprHoles . snd) b
collectExprHoles (EHole _ i ty range) =
  [Hole i ty range]
collectExprHoles _ = []
