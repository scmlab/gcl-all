module GCL.Type2.Unify where

import qualified Data.Map as Map
import Debug.Trace
import GCL.Common (occurs)
import GCL.Range (Range)
import GCL.Type2.Subst (applySubst)
import GCL.Type2.Types
  ( Result,
    Subst,
    TypeError (..),
    throwError,
    typeInt,
  )
import Pretty
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name, TypeOp (..))
import Prelude hiding (EQ, GT, LT)

unify :: A.Type -> A.Type -> Maybe Range -> Result Subst
unify (A.TBase t1 _) (A.TBase t2 _) _ | t1 == t2 = return mempty
unify (A.TArray _i1 t1 _) (A.TArray _i2 t2 _) r = unify t1 t2 r
unify (A.TArray _i t1 _) (A.TApp (A.TApp (A.TOp (Arrow _)) i _) t2 _) r = do
  s1 <- unify i typeInt r
  s2 <- unify t1 t2 r
  return (s2 <> s1)
unify (A.TOp (Arrow _)) (A.TOp (Arrow _)) _ = return mempty
unify t1@(A.TData n1 _) t2@(A.TData n2 _) r =
  if n1 == n2
    then return mempty
    else throwError $ UnifyFailed t1 t2 r
unify (A.TApp a1 a2 _) (A.TApp b1 b2 _) r = do
  s1 <- unify a1 b1 r
  s2 <- unify (applySubst s1 a2) (applySubst s1 b2) r
  return $ s2 <> s1
unify (A.TVar name _) ty r = unifyVar name ty r
unify ty (A.TVar name _) r = unifyVar name ty r
unify A.TType A.TType _ = return mempty
unify t1 t2 r =
  trace
    (show (pretty t1) <> " != " <> show (pretty t2))
    throwError
    $ UnifyFailed t1 t2 r

unifyVar :: Name -> A.Type -> Maybe Range -> Result Subst
unifyVar name ty range
  | A.TVar name Nothing == ty = return mempty
  | occurs name ty = throwError $ RecursiveType name ty range
  | otherwise =
      let subst = Map.singleton name ty
       in return subst
