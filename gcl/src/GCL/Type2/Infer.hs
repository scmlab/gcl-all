{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Avoid lambda" #-}

module GCL.Type2.Infer where

import Data.List (foldl', intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Loc (Loc (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Debug.Trace
import GCL.Type (TypeError (..))
import GCL.Type2.RSE
import Pretty
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (ArithOp, ChainOp (..), Name (Name), Op (..), TypeOp (..))
import qualified Syntax.Typed.Types as T
import Prelude hiding (GT)

newtype Inference = Inference
  { _counter :: Int
  }

type TyVar = Name

type TmVar = Name

data Scheme
  = Forall [TyVar] A.Type -- ∀α₁, ..., αₙ. t
  deriving (Show)

type Env = Map TmVar Scheme

type Subst = Map TyVar A.Type

composeSubst :: Subst -> Subst -> Subst
s1 `composeSubst` s2 = Map.map (applySubst s1) s2 `Map.union` s1

instance {-# OVERLAPPING #-} Semigroup Subst where
  (<>) = composeSubst

instance {-# OVERLAPPING #-} Show Subst where
  show s = intercalate "\n" $ map (\(tv, ty) -> show tv <> " -> " <> show ty) (Map.toList s)

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
applySubst subst (A.TArray interval ty loc) =
  A.TArray interval (applySubst subst ty) loc
applySubst _ (A.TTuple _) = undefined
applySubst subst (A.TFunc e1 e2 loc) =
  A.TFunc (applySubst subst e1) (applySubst subst e2) loc
applySubst _ ty@A.TOp {} = ty
applySubst subst (A.TApp e1 e2 loc) =
  A.TApp (applySubst subst e1) (applySubst subst e2) loc
applySubst _ ty@A.TData {} = ty
applySubst subst ty@(A.TVar name _) =
  Map.findWithDefault ty name subst
applySubst subst ty@(A.TMetaVar name _) =
  Map.findWithDefault ty name subst

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme subst (Forall vars ty) =
  let -- Subst $ Map.filterWithKey (\k _ -> k `notElem` vars) subst
      -- is too inefficient i think if `vars` becomes long
      -- TODO: change to rename here
      filteredSubst = foldl' (\acc var -> Map.delete var acc) subst vars
   in Forall vars (applySubst filteredSubst ty)

applySubstEnv :: Subst -> Env -> Env
applySubstEnv subst = Map.map (applySubstScheme subst)

-- XXX: wouldn't this operation be extremely expensive?
applySubstExpr :: Subst -> T.Expr -> T.Expr
applySubstExpr subst (T.Lit lit ty loc) = T.Lit lit (applySubst subst ty) loc
applySubstExpr subst (T.Var name ty loc) = T.Var name (applySubst subst ty) loc
applySubstExpr subst (T.Const name ty loc) = T.Const name (applySubst subst ty) loc
applySubstExpr subst (T.Op op ty) = T.Op op (applySubst subst ty)
applySubstExpr subst (T.Chain chain) = T.Chain (applySubstChain subst chain)
applySubstExpr subst (T.App e1 e2 loc) = T.App (applySubstExpr subst e1) (applySubstExpr subst e2) loc
applySubstExpr subst (T.Lam param ty body loc) = T.Lam param (applySubst subst ty) (applySubstExpr subst body) loc
applySubstExpr subst (T.Quant _ _ _ _ _) = undefined
applySubstExpr subst (T.ArrIdx arr index loc) = T.ArrIdx (applySubstExpr subst arr) (applySubstExpr subst index) loc
applySubstExpr subst (T.ArrUpd arr index expr loc) = T.ArrUpd (applySubstExpr subst arr) (applySubstExpr subst index) (applySubstExpr subst expr) loc
applySubstExpr subst (T.Case _ _ _) = undefined
applySubstExpr subst (T.Subst _ _) = undefined

applySubstChain :: Subst -> T.Chain -> T.Chain
applySubstChain subst (T.Pure expr) = T.Pure (applySubstExpr subst expr)
applySubstChain subst (T.More chain op ty expr) = T.More (applySubstChain subst chain) op (applySubst subst ty) (applySubstExpr subst expr)

freshTyVar :: RSE Env Inference TyVar
freshTyVar = do
  n <- gets _counter
  put $ Inference (n + 1)
  return $ Name (Text.pack $ "t" <> show n) NoLoc

freshTVar :: RSE Env Inference A.Type
freshTVar = A.TVar <$> freshTyVar <*> pure NoLoc

-- assuming forall ONLY exists on the outside
instantiate :: Scheme -> RSE Env Inference A.Type
instantiate (Forall tvs ty) = do
  mappings <-
    mapM
      ( \var -> do
          ftv <- freshTVar
          return (var, ftv)
      )
      tvs
  let subst = Map.fromList mappings
  return $ applySubst subst ty

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

freeTypeVarsEnv :: Env -> Set TyVar
freeTypeVarsEnv env =
  foldMap freeTypeVarsScheme (Map.elems env)

freeTypeVarsScheme :: Scheme -> Set TyVar
freeTypeVarsScheme (Forall tvs ty) =
  freeTypeVars ty `Set.difference` Set.fromList tvs -- remove quantified tyvars

generalize :: A.Type -> RSE Env Inference Scheme
generalize ty = do
  env <- ask
  let freeVars = freeTypeVars ty `Set.difference` freeTypeVarsEnv env
  return $ Forall (Set.toAscList freeVars) ty

unify :: A.Type -> A.Type -> Loc -> Result Subst
unify (A.TBase t1 _) (A.TBase t2 _) _ | t1 == t2 = return mempty
unify (A.TArray _i1 t1 _) (A.TArray _i2 t2 _) l = unify t1 t2 l
unify (A.TOp op1) (A.TOp op2) _ | op1 == op2 = return mempty
unify (A.TApp a1 a2 _) (A.TApp b1 b2 _) l = do
  s1 <- unify a1 b1 l
  s2 <- unify (applySubst s1 a2) (applySubst s1 b2) l
  return $ s2 <> s1
unify (A.TVar name _) ty l = unifyVar name ty l
unify ty (A.TVar name _) l = unifyVar name ty l
unify (A.TMetaVar name _) ty l = unifyVar name ty l
unify ty (A.TMetaVar name _) l = unifyVar name ty l
unify t1 t2 l =
  trace
    (show (pretty t1) <> "\n" <> show (pretty t2))
    throwError
    $ UnifyFailed t1 t2 l

unifyVar :: Name -> A.Type -> Loc -> Result Subst
unifyVar name ty loc
  | A.TVar name NoLoc == ty = return mempty
  | checkOccurs name ty = throwError $ RecursiveType name ty loc
  | otherwise =
      let subst = Map.singleton name ty
       in return subst

-- we keep `T.Expr` to prevent repeating calculate the same expression
infer :: A.Expr -> RSE Env Inference (Subst, A.Type, T.Expr)
infer (A.Lit lit loc) = inferLit lit loc
infer (A.Var name loc) = inferVar name loc
infer (A.Const name loc) = inferVar name loc
infer (A.Op op) = do
  (subst, ty, op') <- inferArithOp op
  return (subst, ty, T.Op op' ty)
infer (A.Chain chain) = do
  (subst, ty, chain') <- inferChain chain
  return (subst, ty, T.Chain chain')
infer (A.App e1 e2 loc) = inferApp e1 e2 loc
infer (A.Lam param body loc) = inferLam param body loc
infer (A.Func name clauses loc) = undefined
infer (A.Tuple exprs) = undefined
infer (A.Quant _ _ _ _ _) = undefined
infer (A.RedexKernel _ _ _ _) = undefined
infer (A.RedexShell _ _) = undefined
infer (A.ArrIdx arr index loc) = undefined
infer (A.ArrUpd arr index expr loc) = undefined
infer (A.Case expr clauses loc) = undefined

inferLit :: A.Lit -> Loc -> RSE Env Inference (Subst, A.Type, T.Expr)
inferLit lit loc =
  let ty = A.TBase (A.baseTypeOfLit lit) loc
   in return (mempty, ty, T.Lit lit ty loc)

inferVar :: Name -> Loc -> RSE Env Inference (Subst, A.Type, T.Expr)
inferVar name loc = do
  env <- ask
  case Map.lookup name env of
    Just scheme -> do
      ty <- instantiate scheme
      return (mempty, ty, T.Var name ty loc)
    Nothing ->
      -- trace (show env)
      throwError $ NotInScope name

inferChain :: A.Chain -> RSE Env Inference (Subst, A.Type, T.Chain)
inferChain (A.More chain2@(A.More chain1 op1 e1 l1) op2 e2 l2) = do
  ftv <- A.TVar <$> freshTyVar <*> pure NoLoc
  (chainSubst, _, typedChain) <- inferChain chain2
  (opSubst, opTy, typedOp) <- inferChainOp op2
  -- TODO: make sure we need to instantiate here
  -- ChainOp should already provide all ground types so it's probably unnecessary?
  -- XXX: i think this has redundant work being done due to `inferChain chain2` above?
  (s1, ty1, typedE1) <- local (applySubstEnv chainSubst) (infer e1)
  (s2, ty2, typedE2) <- local (applySubstEnv (chainSubst <> s1)) (infer e1)
  unifiedSubst <- lift $ unify (ty1 `typeToType` ty2 `typeToType` ftv) opTy l2
  -- XXX: no unifiedSubst needed here?
  -- s1 and s2 are inverted from base case?
  let resultSubst = chainSubst <> opSubst <> s1 <> s2
  let typedChain' = T.More typedChain typedOp (applySubst unifiedSubst opTy) typedE2
  return (resultSubst, applySubst resultSubst ftv, typedChain')
inferChain (A.More (A.Pure e1 _l1) op e2 l2) = do
  ftv <- A.TVar <$> freshTyVar <*> pure NoLoc
  (opSubst, opTy, typedOp) <- inferChainOp op
  -- TODO: make sure we need to instantiate here
  -- ChainOp should already provide all ground types so it's probably unnecessary?
  (s1, ty1, typedE1) <- infer e1
  (s2, ty2, typedE2) <- local (applySubstEnv (opSubst <> s1)) (infer e2)
  unifiedSubst <- lift $ unify (ty1 `typeToType` ty2 `typeToType` ftv) opTy l2
  let resultSubst = unifiedSubst <> s2 <> s1 <> opSubst
  let typedChain = T.More (T.Pure typedE1) typedOp (applySubst unifiedSubst opTy) typedE2
  return (resultSubst, applySubst resultSubst ftv, typedChain)
inferChain _ = error "this cannot happen"

inferApp :: A.Expr -> A.Expr -> Loc -> RSE Env Inference (Subst, A.Type, T.Expr)
inferApp e1 e2 loc = do
  ftv <- freshTVar
  (s1, ty1, typedE1) <- infer e1
  (s2, ty2, typedE2) <- local (applySubstEnv s1) (infer e2)

  s3 <- lift $ unify (applySubst s2 ty1) (ty2 `typeToType` ftv) loc
  let resultSubst = s3 <> s2 <> s1

  return (resultSubst, applySubst s3 ftv, T.App (applySubstExpr s2 typedE1) (applySubstExpr s3 typedE2) loc)

inferLam :: Name -> A.Expr -> Loc -> RSE Env Inference (Subst, A.Type, T.Expr)
inferLam param body loc = do
  paramTy <- freshTVar
  (bodySubst, bodyTy, typedBody) <- local (Map.insert param (Forall [] paramTy)) (infer body)

  let paramTy' = applySubst bodySubst paramTy
  let returnTy = paramTy' `typeToType` bodyTy

  return (bodySubst, returnTy, T.Lam param paramTy' typedBody loc)
inferArithOp :: ArithOp -> RSE Env Inference (Subst, A.Type, Op)
inferArithOp op = undefined

inferChainOp :: ChainOp -> RSE Env Inference (Subst, A.Type, Op)
inferChainOp op = do
  -- XXX: should we restrict types for comparison operations?
  -- original implementation for `GT`, `LT`, etc. limits it to `Int -> Int -> Bool`
  -- but equality is `forall a. a -> a -> Bool`
  ftv <- A.TVar <$> freshTyVar <*> pure NoLoc
  return (mempty, ftv `typeToType` ftv `typeToType` typeBool, ChainOp op)

inferTypeOp :: TypeOp -> RSE Env Inference (Subst, A.Type, Op)
inferTypeOp op = undefined

-- | construct a type of `a -> b`
typeToType :: A.Type -> A.Type -> A.Type
typeToType t1 t2 = A.TApp (A.TApp (A.TOp (Arrow NoLoc)) t1 NoLoc) t2 NoLoc

-- XXX: is including `Loc` relevant here?
typeInt :: A.Type
typeInt = A.TBase A.TInt NoLoc

typeBool :: A.Type
typeBool = A.TBase A.TBool NoLoc
