{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Avoid lambda" #-}

module GCL.Type2.Infer where

import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import Data.Loc (Loc (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Debug.Trace
import GCL.Type (TypeError (..))
import GCL.Type2.MiniAst
import GCL.Type2.RSE
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (ArithOp, ChainOp, Name (Name), Op (..))

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

instance {-# OVERLAPPING #-} Semigroup Subst where
  s1 <> s2 = Map.map (applySubst s1) s2 `Map.union` s1

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

collectDeclToEnv :: A.Declaration -> Result Env
collectDeclToEnv (A.ConstDecl names ty _ _) = do
  checkDuplicateNames names
  return $ Map.fromList $ map (\name -> (name, Forall [] ty)) names
collectDeclToEnv (A.VarDecl names ty _ _) = do
  checkDuplicateNames names
  return $ Map.fromList $ map (\name -> (name, Forall [] ty)) names

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
      filteredSubst = foldl' (\acc var -> Map.delete var acc) subst vars -- XXX: why? what does this do?
   in Forall vars (applySubst filteredSubst ty)

applySubstEnv :: Subst -> Env -> Env
applySubstEnv subst = Map.map (applySubstScheme subst)

freshTyVar :: RSE Env Inference TyVar
freshTyVar = do
  n <- gets _counter
  put $ Inference (n + 1)
  return $ Name (Text.pack $ "t" <> show n) Nothing

instantiate :: Scheme -> RSE Env Inference A.Type
instantiate (Forall tvs ty) = do
  mappings <-
    mapM
      ( \var -> do
          fresh <- freshTyVar
          return (var, A.TVar fresh NoLoc)
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
unify (A.TVar name _) ty l = unifyVar name ty l
unify ty (A.TVar name _) l = unifyVar name ty l
unify (A.TMetaVar name _) ty l = unifyVar name ty l
unify ty (A.TMetaVar name _) l = unifyVar name ty l
unify t1 t2 l = throwError $ UnifyFailed t1 t2 l

unifyVar :: Name -> A.Type -> Loc -> Result Subst
unifyVar name ty loc
  | A.TVar name NoLoc == ty = return mempty
  | checkOccurs name ty = throwError $ RecursiveType name ty loc
  | otherwise =
      let subst = Map.singleton name ty
       in return subst

infer :: A.Expr -> RSE Env Inference (Subst, A.Type)
infer (A.Lit lit loc) = inferLit lit loc
infer (A.Var name loc) = inferVar name loc
infer (A.Const name loc) = inferVar name loc
infer (A.Op op) = inferArithOp op
infer (A.Chain chain) = trace ("\n" <> show chain <> "\n") $ inferChain chain
infer (A.App e1 e2 loc) = undefined
infer (A.Lam name expr loc) = undefined
infer (A.Func name clauses loc) = undefined
infer (A.Tuple exprs) = undefined
infer (A.Quant _ _ _ _ _) = undefined
infer (A.RedexKernel _ _ _ _) = undefined
infer (A.RedexShell _ _) = undefined
infer (A.ArrIdx arr index loc) = undefined
infer (A.ArrUpd arr index expr loc) = undefined
infer (A.Case expr clauses loc) = undefined

inferLit :: A.Lit -> Loc -> RSE Env Inference (Subst, A.Type)
inferLit lit loc =
  let ty = A.TBase (A.baseTypeOfLit lit) loc
   in return (mempty, ty)

inferVar :: Name -> Loc -> RSE Env Inference (Subst, A.Type)
inferVar name loc = do
  env <- ask
  case Map.lookup name env of
    Just scheme -> do
      ty <- instantiate scheme
      return (mempty, ty)
    Nothing ->
      throwError $ NotInScope name

inferChain :: A.Chain -> RSE Env Inference (Subst, A.Type)
inferChain (A.More (A.Pure e1 l1) op e2 l2) = do
  ftv <- freshTyVar
  (opSubst, opTy) <- inferChainOp op

  (s1, ty1) <- infer e1
  (s2, ty2) <- local (applySubstEnv s1) (infer e2)
  undefined
inferChain _ = undefined

inferOp :: Op -> RSE Env Inference (Subst, A.Type)
inferOp (ArithOp op) = inferArithOp op
inferOp (ChainOp op) = inferChainOp op
inferOp (TypeOp _op) = undefined

inferArithOp :: ArithOp -> RSE Env Inference (Subst, A.Type)
inferArithOp op = undefined

inferChainOp :: ChainOp -> RSE Env Inference (Subst, A.Type)
inferChainOp op = undefined
