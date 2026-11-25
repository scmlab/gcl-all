{-# LANGUAGE OverloadedStrings #-}

module GCL.WP.Struct where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Monad (forM_)
import Data.Text (Text)
import GCL.Common (freshName)
import GCL.Range (MaybeRanged (..))
import GCL.WP.Types
import GCL.WP.Util
import Syntax.Abstract.Operator (tInt)
import Syntax.Abstract.Types (Lit (Num), Type (..))
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Substitution
import Syntax.Typed
import Syntax.Typed.Instances.Substitution ()
import Syntax.Typed.Operator
  ( conj,
    conjunct,
    disjunct,
    eqq,
    gte,
    implies,
    lt,
    neg,
  )
import Syntax.Typed.Util (declaredNamesTypes, getGuards)

type RecFns = (TwpSegs, TwpSStmts, Twp, TspSStmts)

structFunctions ::
  (TwpSegs, TwpSStmts, Twp, TspSStmts) ->
  (TstructStmts, TstructSegs, Tstruct)
structFunctions (wpSegs, wpSStmts, wp, spSStmts) =
  (structStmts, structSegs, struct)
  where
    -- handles  {P} [Stmt] {Q}  where
    -- [Stmt] may contain assertions, invariants, specs, and other statements
    -- (Pred, Maybe Expr) : precondition and bound
    structStmts :: InfMode -> (Pred, Maybe Expr) -> [Stmt] -> Pred -> WP ()
    structStmts Primary pre stmts post = structSegs pre (groupStmts stmts) post
    structStmts Secondary (pre, _) stmts post = case stripAsserts stmts of
      Nothing -> return () -- skip if the program is incomplete
      Just stmts' -> do
        post' <- wpSStmts stmts' post
        tellPO (maybeRangeOf pre) (pre `implies` post')

    -- handles  {P} [SegElm] {Q}
    structSegs :: (Pred, Maybe Expr) -> [SegElm] -> Pred -> WP ()
    structSegs (pre, _) [] post =
      tellPO (maybeRangeOf pre <|> maybeRangeOf post) (pre `implies` post)
    structSegs (pre, _) (SAsrt (Assert p _) : segs) post = do
      tellPO (maybeRangeOf pre) (pre `implies` p)
      structSegs (p, Nothing) segs post
    structSegs (pre, _) (SAsrt (LoopInvariant p bnd l) : segs) post = do
      tellPO l (pre `implies` p)
      structSegs (p, Just bnd) segs post
    structSegs (pre, bnd) [SStmts ss] post =
      structSStmts (pre, bnd) ss post
    structSegs (pre, bnd) (SStmts ss : SAsrt (Assert p _) : segs) post = do
      structSStmts (pre, bnd) ss p
      structSegs (p, Nothing) segs post
    structSegs (pre, bnd) (SStmts ss : SAsrt (LoopInvariant p bd _) : segs) post =
      do
        structSStmts (pre, bnd) ss p
        structSegs (p, Just bd) segs post
    structSegs (pre, bnd) (SStmts ss : SSpec (Spec _ range tenv) : segs) post = do
      post' <- wpSegs segs post
      pre' <- spSStmts (pre, bnd) ss
      tellSpec range pre' post' tenv
    structSegs (pre, _) (SSpec (Spec _ range tenv) : segs) post = do
      post' <- wpSegs segs post
      tellSpec range pre post' tenv
    structSegs _ _ _ = error "Missing case in structSegs"

    -- 'simple' version of struct stmts -- there are no assertions,
    -- invariants, or specs in the list of statements.

    structSStmts :: (Pred, Maybe Expr) -> [Stmt] -> Pred -> WP ()
    structSStmts (pre, _) [] post = do
      tellPO (maybeRangeOf pre <|> maybeRangeOf post) (pre `implies` post)
    structSStmts (pre, bnd) (stmt : stmts) post = do
      post' <- wpSStmts stmts post
      struct (pre, bnd) stmt post'

    struct :: (Pred, Maybe Expr) -> Stmt -> Pred -> WP ()
    struct (pre, _) s@(Abort l) post = tellPO l . (pre `implies`) =<< wp s post
    struct (pre, _) s@(Skip l) post = tellPO l . (pre `implies`) =<< wp s post
    struct (pre, _) s@(Assign vars exprs l) post =
      tellPO l . (pre `implies`) =<< wp s post
    struct (pre, _) s@(AAssign _ _ _ l) post = do
      tellPO l . (pre `implies`) =<< wp s post
    struct (pre, _) (If gcmds l) post = do
      footPrint
        (IfTotal l)
        (tellPO l (pre `implies` disjunctGuards gcmds))
      footPrint
        (IfBranch l)
        ( forM_ gcmds $ \(GdCmd guard body _) ->
            structStmts Primary (pre `conj` guard, Nothing) body post
        )
    struct (inv, Just bnd) (Do gcmds l) post = do
      let guards = getGuards gcmds
      footPrint
        (LoopInvBase l)
        (tellPO l (conjunct (inv : map neg guards) `implies` post))
      footPrint
        (LoopInvInd l)
        (forM_ gcmds (structGdcmdInduct inv))
      footPrint
        (LoopTermBase l)
        ( tellPO
            l
            ( (inv `conj` disjunct guards)
                `implies` (bnd `gte` Lit (Num 0) tInt Nothing)
            )
        )
      footPrint
        (LoopTermInd l)
        (forM_ gcmds (structGdcmdBnd inv bnd))
    struct (inv, Nothing) (Do gcmds l) post = do
      case l of
        Nothing -> return ()
        Just rng -> throwWarning (MissingBound rng)
      let guards = getGuards gcmds
      footPrint
        (LoopInvBase l)
        (tellPO l (conjunct (inv : map neg guards) `implies` post))
      footPrint
        (LoopInvInd l)
        (forM_ gcmds (structGdcmdInduct inv))
    struct _ Proof {} _ = return ()
    struct (pre, _) (Block prog _) post = structBlock pre prog post
    struct (pre, _) s@(Alloc _ _ l) post = do
      tellPO l . (pre `implies`) =<< wp s post
    struct (pre, _) s@(HLookup _ _ l) post = do
      tellPO l . (pre `implies`) =<< wp s post
    struct (pre, _) s@(HMutate _ _ l) post = do
      tellPO l . (pre `implies`) =<< wp s post
    struct (pre, _) s@(Dispose _ l) post = do
      tellPO l . (pre `implies`) =<< wp s post
    struct _ _ _ = error "missing case in struct"

    structGdcmdInduct :: Pred -> GdCmd -> WP ()
    structGdcmdInduct inv (GdCmd guard body _) =
      structStmts Primary (inv `conj` guard, Nothing) body inv

    structGdcmdBnd :: Pred -> Expr -> GdCmd -> WP ()
    structGdcmdBnd inv bnd (GdCmd guard body _) = withFreshVar tInt $ \oldbnd -> do
      structStmts
        Secondary
        (conjunct [inv, bnd `eqq` oldbnd, guard], Nothing)
        body
        (bnd `lt` oldbnd)

    structBlock :: Pred -> Program -> Pred -> WP ()
    structBlock pre (Program _ decls _props stmts _) post = do
      let localNames = declaredNamesTypes decls
      (xs, ys) <-
        withLocalScopes
          ( \scopes ->
              withScopeExtension
                (map (nameToText . fst) localNames)
                (calcLocalRenaming (concat scopes) localNames)
          )
      stmts' <- subst (toSubst ys) stmts
      withScopeExtension
        (xs ++ map (nameToText . fst . snd) ys)
        (structStmts Primary (pre, Nothing) stmts' post)
      where
        toSubst = map (\(n, (n', t)) -> (n, Var n' t (maybeRangeOf n')))

calcLocalRenaming :: [Text] -> [(Name, Type)] -> WP ([Text], [(Text, (Name, Type))])
calcLocalRenaming _ [] = return ([], [])
calcLocalRenaming scope ((x, t) : xs)
  | tx `elem` scope = do
      x' <- freshName tx x
      second ((tx, (x', t)) :) <$> calcLocalRenaming scope xs
  | otherwise =
      first (tx :) <$> calcLocalRenaming scope xs
  where
    tx = nameToText x

-- debugging

-- pp :: Pretty a => a -> String
-- pp = renderString . layoutPretty defaultLayoutOptions . pretty
