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
import GCL.Type2.Common (Env, Scheme (..))
import GCL.Type2.Infer
import GCL.Type2.RSE
import qualified Hack
import Pretty
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name)
import qualified Syntax.Typed.Types as T

collectDeclToEnv :: A.Declaration -> RSE Env Inference Env
collectDeclToEnv (A.ConstDecl names ty _ _) = do
  lift $ checkDuplicateNames names
  _ <- typeToKind ty
  return $ Map.fromList $ map (\name -> (name, Forall [] ty)) names
collectDeclToEnv (A.VarDecl names ty _ _) = do
  lift $ checkDuplicateNames names
  _ <- typeToKind ty
  return $ Map.fromList $ map (\name -> (name, Forall [] ty)) names

type DefnMap = Map A.Definition Scheme

-- XXX: is checking duplicate definition required here?
collectDefnToEnv :: A.Definition -> RSE Env Inference Env
collectDefnToEnv (A.TypeDefn name args ctors _range) = do
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

        let (tvs, ctorTy) = extractMetaVars nameTy ctorArgs
        let diff = filter (`notElem` args) (nub tvs)

        case diff of
          [] -> return $ Map.insert ctorName (Forall args ctorTy) env'
          (x : _) -> throwError $ NotInScope x
    )
    kindEnv
    ctors
  where
    extractMetaVars baseTy =
      foldr
        ( \argType (ftvs, argTypes) ->
            case argType of
              (A.TMetaVar n _) -> (n : ftvs, A.TVar n (maybeRangeOf n) `typeToType` argTypes)
              ty -> (ftvs, ty `typeToType` argTypes)
        )
        ([], baseTy)
collectDefnToEnv (A.FuncDefnSig name ty _ _) = do
  env <- ask
  case Map.lookup name env of
    Nothing -> return $ Map.singleton name (Forall [] ty)
    Just _ -> throwError $ DuplicatedIdentifiers [name]
collectDefnToEnv (A.FuncDefn name body) = do
  (_, ty, _) <- infer body
  return $ Map.singleton name (Forall [] ty)

class ToTyped a t | a -> t where
  toTyped :: a -> RSE Env Inference t

instance ToTyped A.Program T.Program where
  toTyped (A.Program defns decls exprs stmts range) = do
    traceM $ "defns: " <> show (pretty defns)
    traceM $ "decls: " <> show (pretty decls)
    traceM $ "exprs: " <> show (pretty exprs)
    traceM $ "stmts: " <> show (pretty stmts)
    env <- ask
    declEnv <-
      foldM
        ( \env' decl -> do
            declEnv' <- collectDeclToEnv decl
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
            -- NOTE: definitions have to be in order
            defnEnv <- local (const env') (collectDefnToEnv defn)
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
            local (const newEnv) (toTyped decl)
        )
        decls
    typedExprs <- local (const newEnv) (mapM toTyped exprs)
    typedStmts <- local (const newEnv) (mapM toTyped stmts)
    return $ T.Program typedDefns typedDecls typedExprs typedStmts range

instance ToTyped A.Definition T.Definition where
  toTyped (A.TypeDefn name args ctors range) = return $ T.TypeDefn name args (map toTypedTypeDefnCtor ctors) range
  toTyped (A.FuncDefnSig name ty prop range) = toTypedFuncDefnSig name ty prop range
  toTyped (A.FuncDefn name body) = T.FuncDefn name <$> toTyped body

toTypedTypeDefnCtor :: A.TypeDefnCtor -> T.TypeDefnCtor
toTypedTypeDefnCtor (A.TypeDefnCtor name args) = T.TypeDefnCtor name args

-- FIXME: prop is not needed, remove in the future
toTypedFuncDefnSig :: Name -> A.Type -> Maybe A.Expr -> Maybe Range -> RSE Env Inference T.Definition
toTypedFuncDefnSig name ty prop range = do
  tyKind <- typeToKind ty

  when
    (tyKind /= A.TType)
    (throwError $ UnifyFailed tyKind A.TType range)

  return (T.FuncDefnSig' name tyKind range)

instance ToTyped A.Declaration T.Declaration where
  toTyped (A.ConstDecl names ty prop range) = do
    -- TODO: some kind stuff
    case prop of
      Just p -> do
        p' <- toTyped p
        return $ T.ConstDecl names ty (Just p') range
      Nothing ->
        return $ T.ConstDecl names ty Nothing range
  toTyped (A.VarDecl names ty prop range) = do
    -- TODO: some kind stuff
    case prop of
      Just p -> do
        p' <- toTyped p
        return $ T.VarDecl names ty (Just p') range
      Nothing ->
        return $ T.VarDecl names ty Nothing range

instance ToTyped A.Stmt T.Stmt where
  toTyped (A.Skip range) = return (T.Skip range)
  toTyped (A.Abort range) = return (T.Abort range)
  toTyped (A.Assign names exprs range) = toTypedAssign names exprs range
  toTyped (A.AAssign arr index expr range) = toTypedAAssign arr index expr range
  toTyped (A.Assert expr range) = toTypedAssert expr range
  toTyped (A.LoopInvariant e1 e2 range) = toTypedLoopInvariant e1 e2 range
  toTyped (A.Do gds range) = toTypedDo gds range
  toTyped (A.If gds range) = toTypedIf gds range
  toTyped (A.Spec text range) = T.Spec' text range <$> ask
  toTyped (A.Proof t1 t2 range) = return (T.Proof t1 t2 range)
  toTyped (A.Alloc var exprs range) = toTypedAlloc var exprs range
  toTyped (A.HLookup name expr range) = toTypedHLookup name expr range
  toTyped (A.HMutate left right range) = toTypedHMutate left right range
  toTyped (A.Dispose expr range) = toTypedDispose expr range
  toTyped A.Block {} = undefined

toTypedAssign :: [Name] -> [A.Expr] -> Maybe Range -> RSE Env Inference T.Stmt
toTypedAssign names exprs range
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
      return $ T.Assign names typedExprs range

toTypedAAssign :: A.Expr -> A.Expr -> A.Expr -> Maybe Range -> RSE Env Inference T.Stmt
toTypedAAssign arr index expr range = do
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
          range

  return resultStmt

toTypedAssert :: A.Expr -> Maybe Range -> RSE Env Inference T.Stmt
toTypedAssert expr range = do
  (s1, exprTy, typedExpr) <- infer expr
  s2 <- lift $ unify exprTy typeBool (maybeRangeOf expr)
  return (T.Assert (applySubstExpr (s2 <> s1) typedExpr) range)

toTypedLoopInvariant :: A.Expr -> A.Expr -> Maybe Range -> RSE Env Inference T.Stmt
toTypedLoopInvariant e1 e2 range = do
  (e1Subst, typedE1) <- typeCheck e1 typeBool
  (e2Subst, typedE2) <- local (applySubstEnv e1Subst) (typeCheck e2 typeInt)
  let resultSubst = e2Subst <> e1Subst
  let resultStmt =
        T.LoopInvariant
          (applySubstExpr resultSubst typedE1)
          (applySubstExpr resultSubst typedE2)
          range

  return resultStmt

toTypedDo :: [A.GdCmd] -> Maybe Range -> RSE Env Inference T.Stmt
toTypedDo gds range = do
  typedGds <- mapM toTyped gds
  return (T.Do typedGds range)

toTypedIf :: [A.GdCmd] -> Maybe Range -> RSE Env Inference T.Stmt
toTypedIf gds range = do
  typedGds <- mapM toTyped gds
  return (T.If typedGds range)

toTypedAlloc :: Name -> [A.Expr] -> Maybe Range -> RSE Env Inference T.Stmt
toTypedAlloc var exprs range = do
  env <- ask
  ty <- case Map.lookup var env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ NotInScope var

  s <- lift $ unify ty typeInt range
  (resultSubst, typedExprs) <-
    foldM
      ( \(s', typedExprs') expr -> do
          (exprSubst, typedExpr) <- local (applySubstEnv s') (typeCheck expr typeInt)
          return (exprSubst <> s', typedExpr : typedExprs')
      )
      (s, [])
      exprs

  return (T.Alloc var (map (applySubstExpr resultSubst) typedExprs) range)

toTypedHLookup :: Name -> A.Expr -> Maybe Range -> RSE Env Inference T.Stmt
toTypedHLookup name expr range = do
  env <- ask
  ty <- case Map.lookup name env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ NotInScope name

  s1 <- lift $ unify ty typeInt (maybeRangeOf name)
  (s2, typedExpr) <- local (applySubstEnv s1) (typeCheck expr typeInt)

  return $ T.HLookup name (applySubstExpr (s2 <> s1) typedExpr) range

toTypedHMutate :: A.Expr -> A.Expr -> Maybe Range -> RSE Env Inference T.Stmt
toTypedHMutate left right range = do
  (s1, typedLeft) <- typeCheck left typeInt
  (s2, typedRight) <- local (applySubstEnv s1) (typeCheck right typeInt)
  return $ T.HMutate (applySubstExpr (s2 <> s1) typedLeft) (applySubstExpr (s2 <> s1) typedRight) range

toTypedDispose :: A.Expr -> Maybe Range -> RSE Env Inference T.Stmt
toTypedDispose expr range = do
  (s, typedExpr) <- typeCheck expr typeInt
  return $ T.Dispose (applySubstExpr s typedExpr) range

instance ToTyped A.GdCmd T.GdCmd where
  toTyped (A.GdCmd expr stmts range) = do
    (exprSubst, typedExpr) <- typeCheck expr typeBool
    typedStmts <- mapM toTyped stmts

    return (T.GdCmd (applySubstExpr exprSubst typedExpr) typedStmts range)

instance ToTyped A.Expr T.Expr where
  toTyped expr = do
    (subst, ty, typed) <- infer expr
    -- NOTE: invariant doesn't hold during the inference
    -- but `applySubstExpr` traverses the entire subtree
    -- so it is probably really inefficient to do so when
    -- there is likely a few tyvars needed to be substituted
    let typed' = applySubstExpr subst typed

    return typed'

runToTyped :: (ToTyped a t) => a -> Env -> Either TypeError t
runToTyped a env = evalRSE (toTyped a) env (Inference 0)
