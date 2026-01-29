{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module GCL.Type2.ToTyped where

import Control.Monad (foldM, unless, when)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import GCL.Range (MaybeRanged (maybeRangeOf), Range)
import GCL.Type (TypeError (..))
import GCL.Type2.Infer
import GCL.Type2.RSE
import qualified Hack
import Pretty
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name (Name))
import qualified Syntax.Typed.Types as T

-- TODO: check valid type
-- prevent
-- `con A : Int Int`
collectDeclToEnv :: A.Declaration -> Result Env
collectDeclToEnv (A.ConstDecl names ty _ _) = do
  checkDuplicateNames names
  return $ Map.fromList $ map (\name -> (name, Forall [] ty)) names
collectDeclToEnv (A.VarDecl names ty _ _) = do
  checkDuplicateNames names
  return $ Map.fromList $ map (\name -> (name, Forall [] ty)) names

type DefnMap = Map A.Definition Scheme

-- XXX: is checking duplicate definition required here?
collectDefnToEnv :: A.Definition -> RSE Env Inference Env
collectDefnToEnv (A.TypeDefn name args ctors _loc) = do
  let nameTy = A.TData name (maybeRangeOf name)

  let kind = foldr (\_ acc -> A.TType `typeToType` acc) A.TType args
  let kindEnv = Map.singleton name (Forall [] kind)

  traceM $ show (pretty kind)
  -- \* -> *

  -- Left -> forall l r. l -> Either l r
  -- Either -> * -> * -> *

  -- NOTE: we currently do not allow same datatype name and constructor name
  -- because we store both information in the same environment without distinction
  -- e.g.
  -- `data D a = D a` is not allowed and
  -- `data D a = C a` is allowed
  foldM
    ( \env' (A.TypeDefnCtor ctorName ctorArgs) -> do
        when
          (Map.member ctorName env')
          (throwError $ DuplicatedIdentifiers [ctorName])

        let vars = extractMetaVar ctorArgs
        let diff = filter (`notElem` args) (nub vars)

        case diff of
          [] -> return $ Map.insert ctorName (Forall args (foldr (typeToType . toTVar) nameTy vars)) env'
          (x : _) -> throwError $ NotInScope x
    )
    kindEnv
    ctors
  where
    -- XXX: why is multiple `TMetaVar`s generated as `TApp` by the parser?
    -- FIX: rewrite the implementation when this gets fixed in the future
    extractMetaVar [] = []
    extractMetaVar (x : _) = reverse $ aux x []
      where
        aux (A.TMetaVar n _) vars = n : vars
        aux (A.TApp tys (A.TMetaVar n _) _) vars = n : aux tys vars
        aux _ _ = error "impossible"

    toTVar v = A.TVar v (maybeRangeOf v)
collectDefnToEnv (A.FuncDefnSig name ty _ _) = do
  env <- ask
  case Map.lookup name env of
    Nothing -> return $ Map.singleton name (Forall [] ty)
    Just _ -> undefined
collectDefnToEnv (A.FuncDefn name body) = do
  (_, ty, _) <- infer body
  return $ Map.singleton name (Forall [] ty)

class ToTyped a t | a -> t where
  toTyped :: a -> RSE Env Inference t

instance ToTyped A.Program T.Program where
  toTyped (A.Program defns decls exprs stmts loc) = do
    traceM $ "defns: " <> show (pretty defns)
    traceM $ "decls: " <> show (pretty decls)
    traceM $ "exprs: " <> show (pretty exprs)
    traceM $ "stmts: " <> show (pretty stmts)
    env <- ask
    declEnv <-
      foldM
        ( \env' decl -> do
            declEnv' <- lift $ collectDeclToEnv decl
            let dups = declEnv' `Map.intersection` env'
            unless
              (null dups)
              (throwError $ DuplicatedIdentifiers (Map.keys dups))
            return $ declEnv' <> env'
        )
        env
        decls
    defnEnv <-
      foldM
        ( \env' defn -> do
            -- traceM $ Hack.sshow defn
            -- NOTE: definitions have to be in order
            defnEnv <- local (const env') (collectDefnToEnv defn)
            -- traceM $ show defnEnv
            return $ defnEnv <> env'
        )
        declEnv
        defns

    let newEnv = defnEnv <> declEnv
    traceM $ show newEnv
    typedDefns <-
      mapM
        ( \defn -> do
            -- TODO: check array interval type is int
            local (const newEnv) (toTyped defn)
        )
        defns
    typedDecls <-
      mapM
        ( \decl -> do
            -- traceM $ show decl
            local (const newEnv) (toTyped decl)
        )
        decls
    typedExprs <- local (const newEnv) (mapM toTyped exprs)
    typedStmts <- local (const newEnv) (mapM toTyped stmts)
    return $ T.Program typedDefns typedDecls typedExprs typedStmts loc

instance ToTyped A.Definition T.Definition where
  toTyped (A.TypeDefn name args ctors loc) = return $ T.TypeDefn name args (map toTypedTypeDefnCtor ctors) loc
  toTyped (A.FuncDefnSig name ty prop loc) = undefined
  toTyped (A.FuncDefn name body) = T.FuncDefn name <$> toTyped body

toTypedTypeDefnCtor :: A.TypeDefnCtor -> T.TypeDefnCtor
toTypedTypeDefnCtor (A.TypeDefnCtor name args) = T.TypeDefnCtor name args

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
  toTyped (A.Skip loc) = return (T.Skip loc)
  toTyped (A.Abort loc) = return (T.Abort loc)
  toTyped (A.Assign names exprs loc) = toTypedAssign names exprs loc
  toTyped (A.AAssign arr index expr loc) = toTypedAAssign arr index expr loc
  toTyped (A.Assert expr loc) = toTypedAssert expr loc
  toTyped (A.LoopInvariant e1 e2 loc) = toTypedLoopInvariant e1 e2 loc
  toTyped (A.Do gds loc) = toTypedDo gds loc
  toTyped (A.If gds loc) = toTypedIf gds loc
  toTyped (A.Spec text range) = undefined
  toTyped (A.Proof t1 t2 range) = return (T.Proof t1 t2 range)
  -- the rest exprs are all ints
  toTyped _stmt = trace (show _stmt) undefined

toTypedAssign :: [Name] -> [A.Expr] -> Maybe Range -> RSE Env Inference T.Stmt
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
              -- TODO: doesn't account for `AssignToConst`
              when
                (Map.notMember name env)
                (throwError $ NotInScope name)

              toTyped expr
          )
          assignments
      return $ T.Assign names typedExprs loc

toTypedAAssign :: A.Expr -> A.Expr -> A.Expr -> Maybe Range -> RSE Env Inference T.Stmt
toTypedAAssign arr index expr loc = do
  ftv <- freshTVar
  (sa, typedArr) <- typeCheck arr (typeInt `typeToType` ftv)
  (si, typedIndex) <- local (applySubstEnv sa) (typeCheck index typeInt)
  (se, typedExpr) <- local (applySubstEnv (si <> sa)) (typeCheck expr ftv)

  let resultSubst = se <> si <> sa
  let resultStmt =
        T.AAssign
          (applySubstExpr resultSubst typedArr)
          (applySubstExpr resultSubst typedIndex)
          (applySubstExpr resultSubst typedExpr)
          loc

  return resultStmt

toTypedAssert :: A.Expr -> Maybe Range -> RSE Env Inference T.Stmt
toTypedAssert expr loc = do
  (s1, exprTy, typedExpr) <- infer expr
  s2 <- lift $ unify exprTy typeBool (maybeRangeOf expr)
  return (T.Assert (applySubstExpr (s2 <> s1) typedExpr) loc)

toTypedLoopInvariant :: A.Expr -> A.Expr -> Maybe Range -> RSE Env Inference T.Stmt
toTypedLoopInvariant e1 e2 loc = do
  (e1Subst, typedE1) <- typeCheck e1 typeBool
  (e2Subst, typedE2) <- local (applySubstEnv e1Subst) (typeCheck e2 typeInt)
  let resultSubst = e2Subst <> e1Subst
  let resultStmt =
        T.LoopInvariant
          (applySubstExpr resultSubst typedE1)
          (applySubstExpr resultSubst typedE2)
          loc

  return resultStmt

toTypedDo :: [A.GdCmd] -> Maybe Range -> RSE Env Inference T.Stmt
toTypedDo gds loc = do
  typedGds <- mapM toTyped gds
  return (T.Do typedGds loc)

toTypedIf :: [A.GdCmd] -> Maybe Range -> RSE Env Inference T.Stmt
toTypedIf gds loc = do
  typedGds <- mapM toTyped gds
  return (T.If typedGds loc)

instance ToTyped A.GdCmd T.GdCmd where
  toTyped (A.GdCmd expr stmts loc) = do
    (exprSubst, typedExpr) <- typeCheck expr typeBool
    typedStmts <- mapM toTyped stmts

    return (T.GdCmd (applySubstExpr exprSubst typedExpr) typedStmts loc)

instance ToTyped A.Expr T.Expr where
  toTyped expr = do
    (subst, ty, typed) <- infer expr
    -- NOTE: invariant doesn't hold during the inference
    -- but `applySubstExpr` traverses the entire subtree
    -- so it is probably really inefficient to do so when
    -- there is likely a few tyvars needed to be substituted
    let typed' = applySubstExpr subst typed
    -- traceM $ show (pretty typed')
    -- traceM $ show (pretty ty)
    -- traceM $ show subst
    -- traceM $ "\n" <> Hack.sshow typed <> "\n"
    return typed'

runToTyped :: (ToTyped a t) => a -> Env -> Either TypeError t
runToTyped a env = evalRSE (toTyped a) env (Inference 0)
