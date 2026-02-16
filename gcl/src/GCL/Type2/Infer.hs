{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Avoid lambda" #-}

module GCL.Type2.Infer where

import Control.Monad (foldM, when)
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import GCL.Common (Free (..))
import GCL.Range (MaybeRanged (maybeRangeOf), Range)
import GCL.Type2.Infer.BuiltIn
import GCL.Type2.Subst
import GCL.Type2.Unify
import GCL.Type (TypeError (..))
import GCL.Type2.Types
import Pretty
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (ArithOp (..), Name (Name), Op (..), TypeOp (..))
import qualified Syntax.Typed.Types as T
import Prelude hiding (EQ, GT, LT)

instance {-# OVERLAPPING #-} Show Env where
  show e = intercalate "\n" $ map (\(var, scheme) -> s var <> " :: " <> showScheme scheme) (Map.toList e)
    where
      s :: (Pretty a) => a -> String
      s = show . pretty

      showScheme (A.Forall [] ty) = s ty
      showScheme (A.Forall tyParams ty) = "forall " <> unwords (map s tyParams) <> ". " <> s ty

checkDuplicateNames :: [Name] -> Result ()
checkDuplicateNames names =
  let dups = map NE.head . filter ((> 1) . length) . NE.groupBy (\(Name t1 _) (Name t2 _) -> t1 == t2) $ sort names
   in if null dups
        then Right ()
        else Left $ DuplicatedIdentifiers dups


-- assuming forall ONLY exists on the outside
instantiate :: A.Scheme -> TIMonad A.Type
instantiate (A.Forall tvs ty) = do
  mappings <-
    mapM
      ( \var -> do
          fre <- freshTyVar
          return (var, A.TVar fre Nothing)
      )
      tvs
  let subst = Map.fromList mappings
  return $ applySubst subst ty

generalize :: A.Type -> TIMonad A.Scheme
generalize ty = do
  env <- ask
  let fVars = freeVars ty `Set.difference` freeVars env
  return $ A.Forall (Set.toAscList fVars) ty


-- ===============================================

{-

Summary of Algorithmic Typing Rules

 Type Checking:  Γ ⊢ e : t ↓ s
       pattern:  Γ ⊢p p : t ↓ (s, Γ')

 Type Inference: Γ ⊢ e ↑ (s, t)
          chain: Γ ⊢ch ch ↑ (s, t)
       clauses : Γ ⊢cl p : t -> e ↑ (s, u)
-}


{-
   Γ ⊢ x ↑ (s1, t')
   s'2 = unify (t, t')
   -------------------- Checking
   Γ ⊢ x : t ↓ s2 <> s1
-}

-- the operation `_ : _ ↓ _`
typeCheck :: A.Expr -> A.Type -> TIMonad (Subst, T.Expr)
typeCheck expr ty = do
  (s1, exprTy, typedExpr) <- infer expr
  s2 <- lift $ unify exprTy ty (maybeRangeOf expr)
  return (s2 <> s1, typedExpr)

-- (-->) :: a -> b -> (a --> b)
--

-- * -> *

typeToKind :: A.Type -> TIMonad A.Type
typeToKind A.TBase {} = return A.TType
typeToKind A.TArray {} = return $ A.TType `typeToType` A.TType
typeToKind A.TTuple {} = undefined
typeToKind A.TFunc {} = undefined
typeToKind (A.TOp (Arrow _)) = return $ A.TType `typeToType` A.TType `typeToType` A.TType
typeToKind (A.TData name _) = do
  env <- ask
  case Map.lookup name env of
    Just (A.Forall _ k) -> return k
    Nothing -> throwError $ NotInScope name
typeToKind (A.TVar name _) = do
  env <- ask
  case Map.lookup name env of
    -- BUG: this probably wrong
    Just (A.Forall tyParams _) -> return $ foldr (\_ acc -> A.TType `typeToType` acc) A.TType tyParams -- XXX: is this correct?
    Nothing -> throwError $ NotInScope name
typeToKind A.TMetaVar {} = undefined
typeToKind A.TType = return A.TType
typeToKind (A.TApp t1 t2 _) = do
  t1Kind <- typeToKind t1
  -- BUG: this is wrong, didn't even check k2
  case t1Kind of
    (A.TApp (A.TApp (A.TOp (Arrow _)) k1 _) k2 _) -> do
      _ <- lift $ unify t2 k1 (maybeRangeOf t2)
      return k2
    (A.TApp A.TType _ range) -> throwError $ PatternArityMismatch 2 1 range
    -- \* *
    (A.TApp _ (A.TApp _ _ _) range) -> throwError $ PatternArityMismatch 1 2 range -- (* -> *) (* -> *)
    ty' -> do
      traceM $ show ty'
      error "unknown kind"

-- typeToKind (TApp f x) = do
--  (k1 -> k2) <- typeToKind f
--  typeToKind x == k1
--  return k2

-- (* -> * -> *) (* -> *)

-- we keep `T.Expr` to prevent repeating calculate the same expression
infer :: A.Expr -> TIMonad (Subst, A.Type, T.Expr)
infer (A.Lit lit range) = inferLit lit range
infer (A.Var name range) = inferVar name range
infer (A.Const name range) = inferVar name range
infer (A.Op op) = do
  ty <- getArithOpType op
  return (mempty, ty, T.Op (ArithOp op) ty)
infer (A.Chain chain) = do
  (subst, _ty, chain') <- inferChain chain
  -- HACK: i don't like this
  return (subst, typeBool, T.Chain chain')
infer (A.App e1 e2 range) = inferApp e1 e2 range
infer (A.Lam param body range) = inferLam param body range
infer (A.Tuple exprs) = undefined
infer (A.Quant op args cond expr range) = inferQuant op args cond expr range
infer (A.RedexKernel _ _ _ _) = undefined
infer (A.RedexShell _ _) = undefined
infer (A.ArrIdx arr index range) = inferArrIdx arr index range
infer (A.ArrUpd arr index expr range) = inferArrUpd arr index expr range
infer (A.Case expr clauses range) = inferCase expr clauses range

inferLit :: A.Lit -> Maybe Range -> TIMonad (Subst, A.Type, T.Expr)
inferLit lit range =
  let ty = A.TBase (A.baseTypeOfLit lit) range
   in return (mempty, ty, T.Lit lit ty range)

{-
   x : t ∈ Γ
   -------------- VAR
   Γ ⊢ x ↑ (∅, t)
-}

inferVar :: Name -> Maybe Range -> TIMonad (Subst, A.Type, T.Expr)
inferVar name range = do
  env <- ask
  case Map.lookup name env of
    Just scheme -> do
      ty <- instantiate scheme
      return (mempty, ty, T.Var name ty range)
    Nothing ->
      throwError $ NotInScope name


{-
   Γ ⊢ch ch ↑ (s, t)
   ------------------ Chain-Invoke
   Γ ⊢ ch ↑ (s, Bool)

   -- if it is assumed that (≼) has type a -> a -> Bool,
   -- we wouldn't need to check t1 == t2

   (≼) : t ∈ Γ
   Γ ⊢ e1 ↑ (s1, t1)  s1 Γ ⊢ e2 ↑ (s2, t2)
   s3 = unify (t, s2 t1 -> t2 -> Bool)
   --------------------------------------- Chain-Base
   Γ ⊢ch e1 ≼ e2 ↑ (s3 <> s2 <> s1, s3 t2)

   (≼) : t ∈ Γ
   Γ ⊢ch ch ↑ (s1, t1)  s1 Γ ⊢ e2 ↑ (s2, t2)
   s3 = unify (t, s2 t1 -> t2 -> Bool)
   ----------------------------------------- Chain-Ind
   Γ ⊢ch ch ≼ e2 ↑ (s3 <> s2 <> s1, s3 t2)
-}

-- NOTE: the inferred type of `Chain` should always be `typeBool`
-- because it is a shorthand for chaining comparision operators with `&&`
inferChain :: A.Chain -> TIMonad (Subst, A.Type, T.Chain)
inferChain (A.More chain@A.More {} op2 e2 l2) = do
  -- XXX: this also looks similar to `inferApp`

  -- NOTE: when we encounter `EQ` we get TVar but not polymorphic type schemes
  -- i.e. `t0 -> t0 -> Bool` instead of `forall a. a -> a -> Bool`
  -- but unification still works so we can skip the `generalize` and `instantiate` step
  opTy <- getChainOpType op2
  (chainSubst, ty1, typedChain) <- inferChain chain

  (s2, ty2, typedE2) <- local (applySubstEnv chainSubst) (infer e2)
                      --- SCM:  ty1 -> s2 ty1 ?
  s3 <- lift $ unify opTy (ty1 `typeToType` ty2 `typeToType` typeBool) l2
  let resultSubst = s3 <> s2 <> chainSubst
  let typedChain' = T.More typedChain (ChainOp op2) (applySubst s3 opTy) typedE2

                   --- SCM: ty2 -> s3 ty2?
  return (resultSubst, ty2, typedChain')
inferChain (A.More (A.Pure e1 _l1) op e2 l2) = do
  opTy <- getChainOpType op
  (s1, ty1, typedE1) <- infer e1
  (s2, ty2, typedE2) <- local (applySubstEnv s1) (infer e2)

  -- i think we should check if `ty1` == `ty2` == `opTy`?
  --                  SCM:  ty1 -> s2 ty1 ?
  s3 <- lift $ unify opTy (ty1 `typeToType` ty2 `typeToType` typeBool) l2
  let resultSubst = s3 <> s2 <> s1
  let typedChain = T.More (T.Pure typedE1) (ChainOp op) (applySubst s3 opTy) typedE2

  -- HACK: return the type of the second expr for chain evaluation
  -- it's not actually important what this returns
  -- but i still think this is a bit hacky
  return (resultSubst, ty2, typedChain)
   --- SCM: Should "ty2" be "subst s3 ty2"?
inferChain _ = error "this cannot happen"

{-
   Γ ⊢ e1 ↑ (s1, t1)
   s1 Γ ⊢ e2 ↑ (s2, t2)
   fresh ft
   s3 = unify (s2 t1, t2 -> ft)
   ----------------------------------- App
   Γ ⊢ e1 e2 ↑ (s3 <> s2 <> s1, s3 ft)
-}

inferApp :: A.Expr -> A.Expr -> Maybe Range -> TIMonad (Subst, A.Type, T.Expr)
inferApp e1 e2 range = do
  ftv <- freshTVar
  (s1, ty1, typedE1) <- infer e1
  (s2, ty2, typedE2) <- local (applySubstEnv s1) (infer e2)

  s3 <- lift $ unify (applySubst s2 ty1) (ty2 `typeToType` ftv) range
  let resultSubst = s3 <> s2 <> s1

  return (resultSubst, applySubst s3 ftv, T.App typedE1 typedE2 range)

{-
   fresh tx
   Γ, x : tx ⊢ e ↑ (s, tb)
   -------------------------------- Lam
   Γ ⊢ (λ x -> e) ↑ (s tx -> tb, s)
-}

inferLam :: Name -> A.Expr -> Maybe Range -> TIMonad (Subst, A.Type, T.Expr)
inferLam param body range = do
  paramTy <- freshTVar
  (bodySubst, bodyTy, typedBody) <- local (Map.insert param (A.Forall [] paramTy)) (infer body)

  let paramTy' = applySubst bodySubst paramTy
  let returnTy = paramTy' `typeToType` bodyTy

  return (bodySubst, returnTy, T.Lam param paramTy' typedBody range)

{-
   fresh ti
   Γ, i : ti ⊢ R : Bool ↓ s2
   s2 Γ, i : s2 ti ⊢ B : Bool ↓ s3
   ---------------------------------------------------- Quant-Count
   Γ ⊢ ⟨# i : R : B⟩ ↑ (s3 <> s2, Int)

   fresh a
   Γ ⊢ (⊕) : a -> a -> a ↓ s1
   fresh ti
   s1 Γ, i : ti ⊢ R : Bool ↓ s2
   s2 (s1 Γ), i : s2 ti ⊢ B : s2 (s1 a) ↓ s3
   ---------------------------------------------------- Quant
   Γ ⊢ ⟨⊕ i : R : B⟩ ↑ (s3 <> s2 <> s1, s3 (s2 (s1 a)))
-}

inferQuant :: A.Expr -> [Name] -> A.Expr -> A.Expr -> Maybe Range -> TIMonad (Subst, A.Type, T.Expr)
inferQuant op@(A.Op (Hash _)) bound cond expr range = do
  -- special case for `⟨ # bound : cond : expr ⟩`
  (_, _, typedOp) <- infer op -- I am lazy and this specific path is cheap

  -- introduce new vars
  boundEnv <-
    Map.fromList
      <$> mapM
        ( \b -> do
            v <- freshTVar
            return (b, A.Forall [] v)
        )
        bound

  local
    (\e -> boundEnv <> e)
    ( do
        (condSubst, typedCond) <- typeCheck cond typeBool
        (exprSubst, typedExpr) <- local (applySubstEnv condSubst) (typeCheck expr (applySubst condSubst typeBool))
          -- SCM: |applySubst condSubst typeBool)| is always typeBool, right?

        let resultSubst = exprSubst <> condSubst
        let typedQuant = T.Quant typedOp bound typedCond typedExpr range

        return (resultSubst, typeInt, typedQuant)
    )
inferQuant op bound cond expr range = do
  ftv <- freshTVar

  -- introduce new vars
  boundEnv <-
    Map.fromList
      <$> mapM
        ( \b -> do
            v <- freshTVar
            return (b, A.Forall [] v)
        )
        bound

  local
    (\e -> boundEnv <> e)
    ( do
        (opSubst, typedOp) <- typeCheck op (ftv `typeToType` ftv `typeToType` ftv)
        (condSubst, typedCond) <- local (applySubstEnv opSubst) (typeCheck cond typeBool)
        (exprSubst, typedExpr) <- local (applySubstEnv (condSubst <> opSubst)) (typeCheck expr (applySubst (condSubst <> opSubst) ftv))

        let resultSubst = exprSubst <> condSubst <> opSubst
        let typedQuant = T.Quant typedOp bound typedCond typedExpr range
        return (resultSubst, applySubst resultSubst ftv, typedQuant)
    )

{-
   fresh a
   Γ ⊢ arr : Int -> a ↓ sa
   sa Γ ⊢ e : Int ↓ si
   ---------------------------------- ArrIdx
   Γ ⊢ arr[e] ↑ (si <> sa, si (sa a))
-}

inferArrIdx :: A.Expr -> A.Expr -> Maybe Range -> TIMonad (Subst, A.Type, T.Expr)
inferArrIdx arr index range = do
  -- NOTE: treating `TArray` as `Int -> t`, which is probably true on a type-level sense
  -- but we ignore the checks on intervals if that's required at all

  -- TODO: check interval type is `Int`

  ftv <- freshTVar
  (sa, typedArr) <- typeCheck arr (typeInt `typeToType` ftv)
  (si, typedIndex) <- local (applySubstEnv sa) (typeCheck index typeInt)

  let resultSubst = si <> sa
  return (resultSubst, applySubst si ftv, T.ArrIdx typedArr typedIndex range)
                   --- SCM: I think it should be (si (sa ftv))

--
{-
   fresh a
   Γ ⊢ arr : Int -> a ↓ sa
   sa Γ ⊢ ei : Int ↓ si
   si (sa Γ) ⊢ ev : si (sa a) ↓ sv
   ------------------------------------------------------------- ArrUpd
   Γ ⊢ (arr : ei ↦ ev) ↑ (sv <> si <> sa, Int -> sv (si (sa a)))
-}

-- TODO: verify this is correct
inferArrUpd :: A.Expr -> A.Expr -> A.Expr -> Maybe Range -> TIMonad (Subst, A.Type, T.Expr)
inferArrUpd arr index expr range = do
  -- NOTE: treating `TArray` as `Int -> t`, which is probably true on a type-level sense
  -- but we ignore the checks on intervals if that's required at all

  ftv <- freshTVar
  (sa, typedArr, interval) <- do
    (s1, exprTy, typedExpr) <- infer arr
    let interval' = case exprTy of A.TArray i _ _ -> i; _ -> error "impossible"
    s2 <- lift $ unify exprTy (typeInt `typeToType` ftv) (maybeRangeOf arr)
    return (s2 <> s1, typedExpr, interval')
  (si, typedIndex) <- local (applySubstEnv sa) (typeCheck index typeInt)
  (se, typedExpr) <- local (applySubstEnv (si <> sa)) (typeCheck expr (applySubst si ftv))  -- SCM: I think you need (si (sa ftv))

  let resultSubst = se <> si <> sa
  return (resultSubst, A.TArray interval (applySubst si ftv) range, T.ArrUpd typedArr typedIndex typedExpr range)
        -- SCM: I think you need (se (si (sa ftv)))

{-
  Γ ⊢ e ↑ (s0, t0)
  s0 Γ      ⊢cl p1 : t0 -> e1 ↑ (s1, t1)
  s1 (s0 Γ) ⊢cl p2 : t0 -> e2 ↑ (s2, t2)
  s = unify (t1, t2)
  ---------------------------------------- Case
  Γ ⊢ case e of
        p1 -> e1   ↑ (s<>s2<>s1<>s0, s t2)
        p2 -> e2
-}

inferCase :: A.Expr -> [A.CaseClause] -> Maybe Range -> TIMonad (Subst, A.Type, T.Expr)
inferCase expr clauses range = do
  (exprSubst, exprTy, typedExpr) <- infer expr

  caseFtv <- freshTVar

  (clausesSubst, clausesTy, typedClauses) <- foldM (aux exprTy) (mempty, caseFtv, []) clauses

  let resultSubst = clausesSubst <> exprSubst
  return (resultSubst, clausesTy, T.Case typedExpr (reverse typedClauses) range)
  where
    aux exprTy (accSubst, clauseTy, typedClauses) (A.CaseClause pattern caseExpr) = do
      (s1, clauseTy', typedClause) <- inferClause pattern caseExpr exprTy

      -- NOTE: pattern type checking in done in `bindPattern` so no need for it here
      s2 <- lift $ unify clauseTy clauseTy' (maybeRangeOf caseExpr)

      return (s2 <> s1 <> accSubst, applySubst s2 clauseTy', typedClause : typedClauses)

checkDuplicateBinders :: A.Pattern -> Result ()
checkDuplicateBinders pat = do
  _ <- aux pat []
  return ()
  where
    aux :: A.Pattern -> [Name] -> Result [Name]
    aux (A.PattLit _) _binders = return []
    aux (A.PattBinder name) binders =
      if name `elem` binders
        then throwError $ DuplicatedIdentifiers [name]
        else return [name]
    aux (A.PattWildcard _) _binders = return []
    aux (A.PattConstructor _p ps) binders =
      foldM
        ( \b' p' -> do
            b'' <- aux p' b'
            return (b'' <> b')
        )
        binders
        ps

{-
  Γ ⊢p p : t ↓ (sp, Γ')
  sp Γ, Γ' ⊢ e ↑ (se, te)
  --------------------------------- Case-Clause
  Γ ⊢cl p : t -> e ↑ (se <> sp, te)
-}

inferClause :: A.Pattern -> A.Expr -> A.Type -> TIMonad (Subst, A.Type, T.CaseClause)
inferClause pattern expr ty = do
  lift $ checkDuplicateBinders pattern

  (patSubst, patEnv) <- bindPattern pattern ty

  (exprSubst, exprTy, typedExpr) <- local (\e -> patEnv <> applySubstEnv patSubst e) (infer expr)

  let resultSubst = exprSubst <> patSubst
  return (resultSubst, exprTy, T.CaseClause pattern typedExpr)

bindPattern :: A.Pattern -> A.Type -> TIMonad (Subst, Env)
bindPattern (A.PattLit lit) ty = do
  sub <- lift $ unify (A.TBase (A.baseTypeOfLit lit) (maybeRangeOf lit)) ty (maybeRangeOf ty)
  return (sub, mempty)
bindPattern (A.PattBinder name) ty = do
  let env = Map.singleton name (A.Forall [] ty)
  return (mempty, env)
bindPattern (A.PattWildcard _) _ = return (mempty, mempty)
bindPattern (A.PattConstructor ctorName pats) ty = do
  env <- ask

  patTy <- case Map.lookup ctorName env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ NotInScope ctorName

  let tys = toList patTy
  let argTys = NE.init tys
  let resultTy = NE.last tys

  s <- lift $ unify resultTy ty (maybeRangeOf ctorName)

  when
    (length pats /= length argTys)
    (throwError $ PatternArityMismatch (length pats) (length argTys) (maybeRangeOf pats))

  (patsSubst, patsEnv) <-
    foldM
      ( \(s', e') (pat, argTy) -> do
          (s'', e'') <- bindPattern pat argTy
          return (s'' <> s', e'' <> e')
      )
      (s, mempty)
      (zip pats argTys)

  return (patsSubst, patsEnv)
  where
    toList :: A.Type -> NonEmpty A.Type
    toList (A.TApp (A.TApp (A.TOp (Arrow Nothing)) t1 Nothing) t2 Nothing) = t1 NE.<| toList t2
    toList ty' = ty' NE.:| []

inferTypeOp :: TypeOp -> TIMonad (Subst, A.Type, Op)
inferTypeOp op = undefined
