{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Avoid lambda" #-}

module GCL.Type2.Infer where

import Data.List (foldl', intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Debug.Trace
import GCL.Range (MaybeRanged (maybeRangeOf), Range)
import GCL.Type (TypeError (..))
import GCL.Type2.RSE
import qualified Hack
import Pretty
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (ArithOp (..), ChainOp (..), Name (Name), Op (..), TypeOp (..))
import qualified Syntax.Typed.Types as T
import Prelude hiding (EQ, GT, LT)

newtype Inference = Inference
  { _counter :: Int
  }

type TyVar = Name

type TmVar = Name

data Scheme
  = Forall [TyVar] A.Type -- ∀α₁, ..., αₙ. t
  deriving (Show)

type Env = Map TmVar Scheme

instance {-# OVERLAPPING #-} Show Env where
  show e = intercalate "\n" $ map (\(var, scheme) -> s var <> " :: " <> showScheme scheme) (Map.toList e)
    where
      s :: (Pretty a) => a -> String
      s = show . pretty

      showScheme (Forall [] ty) = s ty
      showScheme (Forall tyParams ty) = "forall " <> unwords (map s tyParams) <> ". " <> s ty

type Subst = Map TyVar A.Type

composeSubst :: Subst -> Subst -> Subst
s1 `composeSubst` s2 = Map.map (applySubst s1) s2 `Map.union` s1

instance {-# OVERLAPPING #-} Semigroup Subst where
  (<>) = composeSubst

instance {-# OVERLAPPING #-} Show Subst where
  show s = intercalate "\n" $ map (\(tv, ty) -> show tv <> ": " <> show ty) (Map.toList s)

checkDuplicateNames :: [Name] -> Result ()
checkDuplicateNames names =
  let dups = map NE.head . filter ((> 1) . length) . NE.groupBy (\(Name t1 _) (Name t2 _) -> t1 == t2) $ names
   in if null dups
        then Right ()
        else Left $ DuplicatedIdentifiers dups

-- checkAssign :: Name -> RSE Env Inference

-- prevent infintite types by checking if the type occurs in itself
checkOccurs :: Name -> A.Type -> Bool
checkOccurs name ty = Set.member name (freeTypeVars ty)

-- TODO: maybe `Subst` type class?
applySubst :: Subst -> A.Type -> A.Type
applySubst _ ty@A.TBase {} = ty
applySubst subst (A.TArray interval ty range) =
  A.TArray interval (applySubst subst ty) range
applySubst _ (A.TTuple _) = undefined
applySubst subst (A.TFunc e1 e2 range) =
  A.TFunc (applySubst subst e1) (applySubst subst e2) range
applySubst _ ty@A.TOp {} = ty
applySubst subst (A.TApp e1 e2 range) =
  A.TApp (applySubst subst e1) (applySubst subst e2) range
applySubst _ ty@A.TData {} = ty
applySubst subst ty@(A.TVar name _) =
  Map.findWithDefault ty name subst
applySubst subst ty@(A.TMetaVar name _) =
  Map.findWithDefault ty name subst
applySubst _ A.TType = A.TType -- XXX: is this correct or this should error

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme subst (Forall vars ty) =
  let -- Subst $ Map.filterWithKey (\k _ -> k `notElem` vars) subst
      -- is too inefficient i think if `vars` becomes long
      -- TODO: change to rename here
      filteredSubst = foldl' (\acc var -> Map.delete var acc) subst vars
   in Forall vars (applySubst filteredSubst ty)

applySubstEnv :: Subst -> Env -> Env
applySubstEnv subst = Map.map (applySubstScheme subst)

-- NOTE: This operation is potentially expensive as it traverses through the entire subtree
-- and attempt to perform substitution regardless the type has been substituted before
applySubstExpr :: Subst -> T.Expr -> T.Expr
applySubstExpr subst (T.Lit lit ty range) = T.Lit lit (applySubst subst ty) range
applySubstExpr subst (T.Var name ty range) = T.Var name (applySubst subst ty) range
applySubstExpr subst (T.Const name ty range) = T.Const name (applySubst subst ty) range
applySubstExpr subst (T.Op op ty) = T.Op op (applySubst subst ty)
applySubstExpr subst (T.Chain chain) = T.Chain (applySubstChain subst chain)
applySubstExpr subst (T.App e1 e2 range) = T.App (applySubstExpr subst e1) (applySubstExpr subst e2) range
applySubstExpr subst (T.Lam param ty body range) = T.Lam param (applySubst subst ty) (applySubstExpr subst body) range
applySubstExpr subst (T.Quant _ _ _ _ _) = undefined
applySubstExpr subst (T.ArrIdx arr index range) = T.ArrIdx (applySubstExpr subst arr) (applySubstExpr subst index) range
applySubstExpr subst (T.ArrUpd arr index expr range) = T.ArrUpd (applySubstExpr subst arr) (applySubstExpr subst index) (applySubstExpr subst expr) range
applySubstExpr subst (T.Case _ _ _) = undefined
applySubstExpr subst (T.Subst _ _) = undefined

applySubstChain :: Subst -> T.Chain -> T.Chain
applySubstChain subst (T.Pure expr) = T.Pure (applySubstExpr subst expr)
applySubstChain subst (T.More chain op ty expr) = T.More (applySubstChain subst chain) op (applySubst subst ty) (applySubstExpr subst expr)

freshTyVar :: RSE Env Inference TyVar
freshTyVar = do
  n <- gets _counter
  put $ Inference (n + 1)
  return $ Name (Text.pack $ "t" <> show n) Nothing

freshTVar :: RSE Env Inference A.Type
freshTVar = A.TVar <$> freshTyVar <*> pure Nothing

-- assuming forall ONLY exists on the outside
instantiate :: Scheme -> RSE Env Inference A.Type
instantiate (Forall tvs ty) = do
  mappings <-
    mapM
      ( \var -> do
          fresh <- freshTyVar
          return (var, A.TVar fresh Nothing)
      )
      tvs
  let subst = Map.fromList mappings
  return $ applySubst subst ty

generalize :: A.Type -> RSE Env Inference Scheme
generalize ty = do
  env <- ask
  let freeVars = freeTypeVars ty `Set.difference` freeTypeVarsEnv env
  return $ Forall (Set.toAscList freeVars) ty

-- the operation `_ : _ ↓ _`
typeCheck :: A.Expr -> A.Type -> RSE Env Inference (Subst, T.Expr)
typeCheck expr ty = do
  (s1, exprTy, typedExpr) <- infer expr
  s2 <- lift $ unify exprTy ty (maybeRangeOf expr)
  return (s2 <> s1, typedExpr)

freeTypeVars :: A.Type -> Set TyVar
freeTypeVars A.TBase {} = mempty
freeTypeVars (A.TArray _ ty _) = freeTypeVars ty
freeTypeVars A.TTuple {} = mempty
freeTypeVars (A.TFunc t1 t2 _) = freeTypeVars t1 <> freeTypeVars t2
freeTypeVars A.TData {} = mempty
freeTypeVars A.TOp {} = mempty
freeTypeVars (A.TApp t1 t2 _) = freeTypeVars t1 <> freeTypeVars t2
freeTypeVars (A.TVar name _) = Set.singleton name
freeTypeVars (A.TMetaVar name _) = Set.singleton name
freeTypeVars A.TType = mempty

freeTypeVarsEnv :: Env -> Set TyVar
freeTypeVarsEnv env =
  foldMap freeTypeVarsScheme (Map.elems env)

freeTypeVarsScheme :: Scheme -> Set TyVar
freeTypeVarsScheme (Forall tvs ty) =
  freeTypeVars ty `Set.difference` Set.fromList tvs -- remove quantified tyvars

unify :: A.Type -> A.Type -> Maybe Range -> Result Subst
unify (A.TBase t1 _) (A.TBase t2 _) _ | t1 == t2 = return mempty
unify (A.TArray _i1 t1 _) (A.TArray _i2 t2 _) l = unify t1 t2 l
unify (A.TArray _i t1 _) (A.TApp (A.TApp (A.TOp (Arrow _)) i _) t2 _) l = do
  s1 <- unify i typeInt l
  s2 <- unify t1 t2 l
  return (s2 <> s1)
unify (A.TOp op1) (A.TOp op2) _ | op1 == op2 = return mempty
unify (A.TData n1 l1) (A.TData n2 l2) _ = undefined
unify (A.TApp a1 a2 _) (A.TApp b1 b2 _) l = do
  s1 <- unify a1 b1 l
  s2 <- unify (applySubst s1 a2) (applySubst s1 b2) l
  return $ s2 <> s1
unify (A.TVar name _) ty l = unifyVar name ty l
unify ty (A.TVar name _) l = unifyVar name ty l
unify A.TType A.TType _ = return mempty
unify t1 t2 l =
  trace
    (show (pretty t1) <> " != " <> show (pretty t2))
    throwError
    $ UnifyFailed t1 t2 l

unifyVar :: Name -> A.Type -> Maybe Range -> Result Subst
unifyVar name ty range
  | A.TVar name Nothing == ty = return mempty
  | checkOccurs name ty = throwError $ RecursiveType name ty range
  | otherwise =
      let subst = Map.singleton name ty
       in return subst

-- (-->) :: a -> b -> (a --> b)
--

-- * -> *

typeToKind :: A.Type -> RSE Env Inference A.Type
typeToKind A.TBase {} = return A.TType
typeToKind A.TArray {} = return $ A.TType `typeToType` A.TType
typeToKind A.TTuple {} = undefined
typeToKind A.TFunc {} = undefined
typeToKind (A.TOp (Arrow _)) = return $ A.TType `typeToType` A.TType `typeToType` A.TType
typeToKind (A.TData name _) = do
  env <- ask
  case Map.lookup name env of
    Just (Forall _ k) -> return k
    Nothing -> throwError $ NotInScope name
typeToKind (A.TVar name _) = do
  env <- ask
  case Map.lookup name env of
    Just (Forall tyParams _) -> return $ foldr (\_ acc -> A.TType `typeToType` acc) A.TType tyParams -- XXX: is this correct?
    Nothing -> throwError $ NotInScope name
typeToKind A.TMetaVar {} = undefined
typeToKind A.TType = return A.TType
typeToKind (A.TApp t1 t2 _) = do
  t1Kind <- typeToKind t1
  case t1Kind of
    (A.TApp (A.TApp (A.TOp (Arrow _)) k1 _) k2 _) -> do
      _ <- lift $ unify t2 k1 (maybeRangeOf t2)
      return k2
    (A.TApp A.TType _ range) -> throwError $ PatternArityMismatch 2 1 range
    ty' -> do
      traceM $ show ty'
      error "unknown kind"

-- typeToKind (TApp f x) = do
--  (k1 -> k2) <- typeToKind f
--  typeToKind x == k1
--  return k2

-- (* -> * -> *) (* -> *)

-- we keep `T.Expr` to prevent repeating calculate the same expression
infer :: A.Expr -> RSE Env Inference (Subst, A.Type, T.Expr)
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
infer (A.Func name clauses range) = undefined
infer (A.Tuple exprs) = undefined
infer expr@(A.Quant _ _ _ _ _) = trace (show expr) undefined
infer (A.RedexKernel _ _ _ _) = undefined
infer (A.RedexShell _ _) = undefined
infer (A.ArrIdx arr index range) = inferArrIdx arr index range
infer (A.ArrUpd arr index expr range) = inferArrUpd arr index expr range
infer (A.Case expr clauses range) = inferCase expr clauses range

inferLit :: A.Lit -> Maybe Range -> RSE Env Inference (Subst, A.Type, T.Expr)
inferLit lit range =
  let ty = A.TBase (A.baseTypeOfLit lit) range
   in return (mempty, ty, T.Lit lit ty range)

inferVar :: Name -> Maybe Range -> RSE Env Inference (Subst, A.Type, T.Expr)
inferVar name range = do
  env <- ask
  case Map.lookup name env of
    Just scheme -> do
      ty <- instantiate scheme
      return (mempty, ty, T.Var name ty range)
    Nothing ->
      throwError $ NotInScope name

-- NOTE: the inferred type of `Chain` should always be `typeBool`
-- because it is a shorthand for chaining comparision operators with `&&`
inferChain :: A.Chain -> RSE Env Inference (Subst, A.Type, T.Chain)
inferChain (A.More chain@A.More {} op2 e2 l2) = do
  -- XXX: this also looks similar to `inferApp`

  -- NOTE: when we encounter `EQ` we get TVar but not polymorphic type schemes
  -- i.e. `t0 -> t0 -> Bool` instead of `forall a. a -> a -> Bool`
  -- but unification still works so we can skip the `generalize` and `instantiate` step
  opTy <- getChainOpType op2
  (chainSubst, ty1, typedChain) <- inferChain chain

  (s2, ty2, typedE2) <- local (applySubstEnv chainSubst) (infer e2)

  s3 <- lift $ unify opTy (ty1 `typeToType` ty2 `typeToType` typeBool) l2
  let resultSubst = s3 <> s2 <> chainSubst
  let typedChain' = T.More typedChain (ChainOp op2) (applySubst s3 opTy) typedE2

  return (resultSubst, ty2, typedChain')
inferChain (A.More (A.Pure e1 _l1) op e2 l2) = do
  opTy <- getChainOpType op
  (s1, ty1, typedE1) <- infer e1
  (s2, ty2, typedE2) <- local (applySubstEnv s1) (infer e2)

  -- i think we should check if `ty1` == `ty2` == `opTy`?
  s3 <- lift $ unify opTy (ty1 `typeToType` ty2 `typeToType` typeBool) l2
  let resultSubst = s3 <> s2 <> s1
  let typedChain = T.More (T.Pure typedE1) (ChainOp op) (applySubst s3 opTy) typedE2

  -- HACK: return the type of the second expr for chain evaluation
  -- it's not actually important what this returns
  -- but i still think this is a bit hacky
  return (resultSubst, ty2, typedChain)
inferChain _ = error "this cannot happen"

inferApp :: A.Expr -> A.Expr -> Maybe Range -> RSE Env Inference (Subst, A.Type, T.Expr)
inferApp e1 e2 range = do
  ftv <- freshTVar
  (s1, ty1, typedE1) <- infer e1
  (s2, ty2, typedE2) <- local (applySubstEnv s1) (infer e2)

  s3 <- lift $ unify (applySubst s2 ty1) (ty2 `typeToType` ftv) range
  let resultSubst = s3 <> s2 <> s1

  return (resultSubst, applySubst s3 ftv, T.App typedE1 typedE2 range)

inferLam :: Name -> A.Expr -> Maybe Range -> RSE Env Inference (Subst, A.Type, T.Expr)
inferLam param body range = do
  paramTy <- freshTVar
  (bodySubst, bodyTy, typedBody) <- local (Map.insert param (Forall [] paramTy)) (infer body)

  let paramTy' = applySubst bodySubst paramTy
  let returnTy = paramTy' `typeToType` bodyTy

  return (bodySubst, returnTy, T.Lam param paramTy' typedBody range)

inferArrIdx :: A.Expr -> A.Expr -> Maybe Range -> RSE Env Inference (Subst, A.Type, T.Expr)
inferArrIdx arr index range = do
  -- NOTE: treating `TArray` as `Int -> t`, which is probably true on a type-level sense
  -- but we ignore the checks on intervals if that's required at all

  -- TODO: check interval type is `Int`

  ftv <- freshTVar
  (sa, typedArr) <- typeCheck arr (typeInt `typeToType` ftv)
  (si, typedIndex) <- local (applySubstEnv sa) (typeCheck index typeInt)

  let resultSubst = si <> sa
  return (resultSubst, applySubst si ftv, T.ArrIdx typedArr typedIndex range)

-- TODO: verify this is correct
inferArrUpd :: A.Expr -> A.Expr -> A.Expr -> Maybe Range -> RSE Env Inference (Subst, A.Type, T.Expr)
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
  (se, typedExpr) <- local (applySubstEnv (si <> sa)) (typeCheck expr (applySubst si ftv))

  let resultSubst = se <> si <> sa
  return (resultSubst, A.TArray interval (applySubst si ftv) range, T.ArrUpd typedArr typedIndex typedExpr range)

inferCase :: A.Expr -> [A.CaseClause] -> Maybe Range -> RSE Env Inference (Subst, A.Type, T.Expr)
inferCase expr clauses range = do
  undefined

pat :: A.Pattern -> A.Type -> RSE Env Inference (Env, Subst)
pat (A.PattLit lit) ty = do
  sub <- lift $ unify (A.TBase (A.baseTypeOfLit lit) (maybeRangeOf lit)) ty (maybeRangeOf ty)
  return (mempty, sub)
pat (A.PattBinder name) ty = do
  let env = Map.singleton name (Forall [] ty)
  return (env, mempty)
pat (A.PattWildcard _) _ = return (mempty, mempty)
pat (A.PattConstructor p ps) ty = do
  env <- ask
  _ <- case Map.lookup p env of
    Nothing -> throwError $ NotInScope p
    Just _ -> undefined
  undefined

getArithOpType :: ArithOp -> RSE Env Inference A.Type
getArithOpType Implies {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType ImpliesU {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType Conj {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType ConjU {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType Disj {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType DisjU {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType Neg {} = return (typeBool `typeToType` typeBool)
getArithOpType NegU {} = return (typeBool `typeToType` typeBool)
getArithOpType NegNum {} = return (typeInt `typeToType` typeInt)
getArithOpType Add {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Sub {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Mul {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Div {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Mod {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Max {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Min {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Exp {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Hash {} = return (typeBool `typeToType` typeInt)
getArithOpType PointsTo {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType SConj {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType SImp {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)

getChainOpType :: ChainOp -> RSE Env Inference A.Type
getChainOpType EQProp {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getChainOpType EQPropU {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getChainOpType EQ {} = do
  ftv <- freshTVar
  return (ftv `typeToType` ftv `typeToType` typeBool)
getChainOpType NEQ {} = do
  ftv <- freshTVar
  return (ftv `typeToType` ftv `typeToType` typeBool)
getChainOpType NEQU {} = do
  ftv <- freshTVar
  return (ftv `typeToType` ftv `typeToType` typeBool)
getChainOpType LT {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)
getChainOpType LTE {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)
getChainOpType LTEU {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)
getChainOpType GT {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)
getChainOpType GTE {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)
getChainOpType GTEU {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)

inferTypeOp :: TypeOp -> RSE Env Inference (Subst, A.Type, Op)
inferTypeOp op = undefined

infixr 1 `typeToType`

-- | construct a type of `a -> b`
typeToType :: A.Type -> A.Type -> A.Type
typeToType t1 t2 = A.TApp (A.TApp (A.TOp (Arrow Nothing)) t1 Nothing) t2 Nothing

-- XXX: is including `Loc` relevant here?
typeInt :: A.Type
typeInt = A.TBase A.TInt Nothing

typeBool :: A.Type
typeBool = A.TBase A.TBool Nothing
