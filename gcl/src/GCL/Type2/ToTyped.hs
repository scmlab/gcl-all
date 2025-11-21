{-# LANGUAGE FunctionalDependencies #-}

module GCL.Type2.ToTyped where

import Control.Monad (foldM, unless, when)
import Data.Loc (Loc)
import qualified Data.Map as Map
import Debug.Trace
import GCL.Type (TypeError (..))
import GCL.Type2.Infer
import GCL.Type2.RSE
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name, Op (..))
import qualified Syntax.Typed.Types as T

class ToTyped a t | a -> t where
  toTyped :: a -> RSE Env Inference t

instance ToTyped A.Program T.Program where
  toTyped (A.Program defns decls exprs stmts loc) = do
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
            toTyped defn
        )
        defns
    typedDecls <-
      mapM
        ( \decl -> do
            toTyped decl
        )
        decls
    typedExprs <-
      mapM
        ( \expr -> do
            toTyped expr
        )
        exprs
    typedStmts <-
      mapM
        ( \stmt -> do
            local (const declEnv) (toTyped stmt)
        )
        stmts
    return $ T.Program typedDefns typedDecls typedExprs typedStmts loc

instance ToTyped A.Definition T.Definition where
  toTyped = undefined

instance ToTyped A.Declaration T.Declaration where
  toTyped (A.ConstDecl names ty prop loc) = do
    -- TODO: some kind stuff
    case prop of
      Just p -> do
        p' <- toTyped p
        return $ T.ConstDecl names ty (Just p') loc
      Nothing ->
        return $ T.ConstDecl names ty Nothing loc
  toTyped (A.VarDecl names ty prop loc) = do
    -- TODO: some kind stuff
    case prop of
      Just p -> do
        p' <- toTyped p
        return $ T.VarDecl names ty (Just p') loc
      Nothing ->
        return $ T.VarDecl names ty Nothing loc

instance ToTyped A.Stmt T.Stmt where
  toTyped (A.Assign names exprs loc) = toTypedAssign names exprs loc
  toTyped _stmt = undefined

toTypedAssign :: [Name] -> [A.Expr] -> Loc -> RSE Env Inference T.Stmt
toTypedAssign names exprs loc
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

              toTyped expr
          )
          assignments
      return $ T.Assign names typedExprs loc

instance ToTyped A.Expr T.Expr where
  toTyped expr = do
    (_, ty, typed) <- infer expr
    return typed

runToTyped :: (ToTyped a t) => a -> Env -> Either TypeError t
runToTyped a env = evalRSE (toTyped a) env (Inference 0)
