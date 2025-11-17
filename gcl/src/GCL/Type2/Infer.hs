{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module GCL.Type2.Infer where

import qualified Data.List.NonEmpty as NE
import Data.Loc (Loc)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceM)
import GCL.Type (TypeError (..))
import GCL.Type2.RSE
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name (Name))

newtype Inference = Inference
  { _counter :: Int
  }

type TyVar = Name

type TmVar = Name

data Scheme
  = Forall [TyVar] A.Type
  deriving (Show)

type Env = Map TmVar Scheme

newtype Subst = Subst (Map TyVar A.Type)

instance Semigroup Subst where
  s1@(Subst m1) <> (Subst m2) = Subst $ Map.map (applySubst s1) m2 `Map.union` m1

instance Monoid Subst where
  mempty = Subst Map.empty

checkDuplicateNames :: [Name] -> Result ()
checkDuplicateNames names =
  let dups = map NE.head . filter ((> 1) . length) . NE.groupBy (\(Name t1 _) (Name t2 _) -> t1 == t2) $ names
   in if null dups
        then Right ()
        else Left $ DuplicatedIdentifiers dups

-- checkAssign :: Name -> RSE Env Inference

collectDeclToEnv :: A.Declaration -> Result Env
collectDeclToEnv (A.ConstDecl names ty _ _) = do
  checkDuplicateNames names
  return $ Map.fromList $ map (\name -> (name, Forall [] ty)) names
collectDeclToEnv (A.VarDecl names ty _ _) = do
  checkDuplicateNames names
  return $ Map.fromList $ map (\name -> (name, Forall [] ty)) names

applySubst :: Subst -> A.Type -> A.Type
applySubst = undefined

freshTyVar :: RSE Env Inference Text
freshTyVar = do
  n <- gets _counter
  put $ Inference (n + 1)
  return . Text.pack $ "t" <> show n

instantiate :: Scheme -> RSE Env Inference A.Type
instantiate scheme = do
  _

unify :: A.Type -> A.Type -> Loc -> Result Subst
unify (A.TBase t1 _) (A.TBase t2 _) _ | t1 == t2 = return mempty
unify t1 t2 l = undefined

infer :: A.Expr -> RSE Env Inference (Subst, A.Type)
infer (A.Lit lit loc) = inferLit lit loc
infer (A.Var name loc) = inferVar name loc
infer (A.Const name loc) = inferVar name loc
infer (A.Op op) = undefined
infer (A.Chain chain) = undefined
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
