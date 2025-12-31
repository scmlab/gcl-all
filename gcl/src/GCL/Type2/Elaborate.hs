{-# LANGUAGE FunctionalDependencies #-}

module GCL.Type2.Elaborate where

import Control.Monad (foldM, unless, when)
import GCL.Range (Range)
import qualified Data.Map as Map
import Debug.Trace
import GCL.Type (TypeError (..))
import GCL.Type2.Infer
import GCL.Type2.RSE
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name, Op (..))
import qualified Syntax.Typed.Types as T

class Elaborate a t | a -> t where
  elaborate :: a -> RSE Env Inference t

instance Elaborate A.Program T.Program where
  elaborate (A.Program defns decls exprs stmts loc) = do
    env <- ask
    declEnv <-
      foldM
        ( \env' decl -> do
            declEnv' <- lift $ collectDeclToEnv decl
            let dups = Map.keys $ declEnv' `Map.intersection` env'
            unless
              (null dups)
              (throwError $ DuplicatedIdentifiers dups)
            return $ declEnv' <> env'
        )
        env
        decls
    typedDefns <-
      mapM
        ( \defn -> do
            elaborate defn
        )
        defns
    typedDecls <-
      mapM
        ( \decl -> do
            elaborate decl
        )
        decls
    typedExprs <-
      mapM
        ( \expr -> do
            elaborate expr
        )
        exprs
    typedStmts <-
      mapM
        ( \stmt -> do
            local (const declEnv) (elaborate stmt)
        )
        stmts
    return $ T.Program typedDefns typedDecls typedExprs typedStmts loc

instance Elaborate A.Definition T.Definition where
  elaborate = undefined

instance Elaborate A.Declaration T.Declaration where
  elaborate (A.ConstDecl names ty prop loc) = do
    -- TODO: some kind stuff
    case prop of
      Just p -> do
        p' <- elaborate p
        return $ T.ConstDecl names ty (Just p') loc
      Nothing ->
        return $ T.ConstDecl names ty Nothing loc
  elaborate (A.VarDecl names ty prop loc) = do
    -- TODO: some kind stuff
    case prop of
      Just p -> do
        p' <- elaborate p
        return $ T.VarDecl names ty (Just p') loc
      Nothing ->
        return $ T.VarDecl names ty Nothing loc

instance Elaborate A.Stmt T.Stmt where
  elaborate (A.Assign names exprs loc) = elaborateAssign names exprs loc
  elaborate _stmt = undefined

elaborateAssign :: [Name] -> [A.Expr] -> Maybe Range -> RSE Env Inference T.Stmt
elaborateAssign names exprs loc
  | length names > length exprs = throwError $ RedundantNames (drop (length exprs) names)
  | length names < length exprs = throwError $ RedundantExprs (drop (length names) exprs)
  | otherwise = do
      env <- ask
      lift $ checkDuplicateNames names
      let assignments = zip names exprs
      typedExprs <-
        mapM
          ( \(name, expr) -> do
              traceM $ show name <> show (Map.lookup name env)
              -- TODO: doesn't account for `AssignToConst`
              when
                (Map.notMember name env)
                (throwError $ NotInScope name)

              elaborate expr
          )
          assignments
      return $ T.Assign names typedExprs loc

instance Elaborate A.Expr T.Expr where
  elaborate expr = do
    (_, ty) <- infer expr
    return $ toTypedExpr expr ty
    where
      toTypedExpr (A.Lit lit loc) ty = T.Lit lit ty loc
      toTypedExpr (A.Var name loc) ty = T.Var name ty loc
      toTypedExpr (A.Const name loc) ty = T.Const name ty loc -- XXX: should expr distinguish var and const?
      toTypedExpr (A.Op op) ty = T.Op (ArithOp op) ty
      toTypedExpr (A.Chain chain) ty = undefined
      toTypedExpr (A.App e1 e2 loc) ty = undefined
      toTypedExpr (A.Lam name expr' loc) ty = undefined
      toTypedExpr (A.Func name clauses loc) ty = undefined
      toTypedExpr (A.Tuple exprs) ty = undefined
      toTypedExpr (A.Quant _ _ _ _ _) ty = undefined
      toTypedExpr (A.RedexKernel _ _ _ _) ty = undefined
      toTypedExpr (A.RedexShell _ _) ty = undefined
      toTypedExpr (A.ArrIdx arr index loc) ty = undefined
      toTypedExpr (A.ArrUpd arr index expr' loc) ty = undefined
      toTypedExpr (A.Case expr' clauses loc) ty = undefined

runElaboration :: (Elaborate a t) => a -> Env -> Either TypeError t
runElaboration a env = evalRSE (elaborate a) env (Inference 0)
