{-# LANGUAGE FlexibleInstances #-}

-- SCM: This is temporary. Might switch to system-wise Syntax.Substitution.
module GCL.Type2.Subst where

import Data.List (foldl', intercalate)
import qualified Data.Map as Map
import GCL.Type2.Types
import Pretty
import qualified Syntax.Abstract.Types as A
import qualified Syntax.Typed.Types as T
import Prelude hiding (EQ, GT, LT)

composeSubst :: Subst -> Subst -> Subst
s1 `composeSubst` s2 = Map.map (applySubst s1) s2 `Map.union` s1

instance {-# OVERLAPPING #-} Semigroup Subst where
  (<>) = composeSubst

instance {-# OVERLAPPING #-} Show Subst where
  show s = intercalate "\n" $ map (\(tv, ty) -> show tv <> ": " <> show ty) (Map.toList s)

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

applySubstScheme :: Subst -> A.Scheme -> A.Scheme
applySubstScheme subst (A.Forall vars ty) =
  let -- Subst $ Map.filterWithKey (\k _ -> k `notElem` vars) subst
      -- is too inefficient i think if `vars` becomes long
      -- TODO: change to rename here
      filteredSubst = foldl' (\acc var -> Map.delete var acc) subst vars
   in A.Forall vars (applySubst filteredSubst ty)

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
applySubstExpr subst (T.Case expr clauses range) = T.Case (applySubstExpr subst expr) (map (applySubstClause subst) clauses) range
applySubstExpr subst (T.Subst _ _) = undefined

applySubstChain :: Subst -> T.Chain -> T.Chain
applySubstChain subst (T.Pure expr) = T.Pure (applySubstExpr subst expr)
applySubstChain subst (T.More chain op ty expr) = T.More (applySubstChain subst chain) op (applySubst subst ty) (applySubstExpr subst expr)

applySubstClause :: Subst -> T.CaseClause -> T.CaseClause
applySubstClause subst (T.CaseClause pat expr) = T.CaseClause pat (applySubstExpr subst expr)
