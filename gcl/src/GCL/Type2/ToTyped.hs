{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GCL.Type2.ToTyped where

import Control.Monad (foldM, void, when)
import Data.Graph (SCC)
import qualified Data.Graph as Graph
import Data.List (foldl', nub)
import qualified Data.Map as Map
import Debug.Trace
import qualified GCL.Dependency as D
import GCL.Range (MaybeRanged (maybeRangeOf), Range)
import GCL.Type2.Infer (checkDuplicateNames, infer, instantiate, typeCheck, typeToKind)
import GCL.Type2.Subst (applySubst, applySubstEnv, applySubstExpr)
import GCL.Type2.Types
  ( Env,
    TIMonad,
    TypeError (..),
    ask,
    evalTI,
    freshTVar,
    lift,
    local,
    throwError,
    typeBool,
    typeInt,
    typeToType,
  )
import GCL.Type2.Unify (unify)
import Pretty
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name)
import qualified Syntax.Typed.Types as T

collectDeclToEnv :: A.Declaration -> TIMonad (Env, T.Declaration)
collectDeclToEnv (A.ConstDecl names ty prop range) = do
  case ty of
    (A.TArray interval _ _) -> validateInterval interval
    _ -> return ()

  _ <- typeToKind ty

  let resultEnv = Map.fromList $ map (\name -> (name, A.Forall [] ty)) names

  resultDecl <-
    case prop of
      Just p -> do
        p' <- toTyped p
        return $ T.ConstDecl names ty (Just p') range
      Nothing ->
        return $ T.ConstDecl names ty Nothing range
  return (resultEnv, resultDecl)
collectDeclToEnv (A.VarDecl names ty prop range) = do
  case ty of
    (A.TArray interval _ _) -> validateInterval interval
    _ -> return ()

  _ <- typeToKind ty

  let resultEnv = Map.fromList $ map (\name -> (name, A.Forall [] ty)) names

  resultDecl <-
    case prop of
      Just p -> do
        p' <- toTyped p
        return $ T.VarDecl names ty (Just p') range
      Nothing ->
        return $ T.VarDecl names ty Nothing range
  return (resultEnv, resultDecl)

validateInterval :: A.Interval -> TIMonad ()
validateInterval (A.Interval begin end _) = validateEndpoint begin >> validateEndpoint end
  where
    validateEndpoint (A.Including expr) = void (typeCheck expr (A.TBase A.TInt Nothing))
    validateEndpoint (A.Excluding expr) = void (typeCheck expr (A.TBase A.TInt Nothing))

collectSccDefns :: SCC A.Definition -> TIMonad (Env, [T.Definition])
collectSccDefns defns = do
  env <- ask
  let names = fmap (\case (A.TypeDefn name _ _ _) -> name; (A.ValDefn name _ _) -> name) defns

  stubEnv <-
    foldM
      ( \accEnv name -> do
          ftv <- freshTVar
          return (Map.singleton name (A.Forall [] ftv) <> accEnv)
      )
      mempty
      names

  foldM
    ( \(accEnv, accDefns) defn -> do
        (env', typedDefn) <- local (const accEnv) (collectDefnToEnv defn)
        return (env' <> accEnv, typedDefn : accDefns)
    )
    (stubEnv <> env, [])
    defns

collectDefnToEnv :: A.Definition -> TIMonad (Env, T.Definition)
collectDefnToEnv (A.TypeDefn name args ctors range) = do
  let nameTy = A.TData name (maybeRangeOf name)
  let resultTy = foldl' (\accTy arg -> A.TApp accTy (A.TVar arg Nothing) Nothing) nameTy args

  let kind = foldl' (\acc _ -> A.TType `typeToType` acc) A.TType args
  let kindEnv = Map.singleton name (A.Forall [] kind)

  -- NOTE: we currently do not allow same datatype name and constructor name
  -- because we store both information in the same environment without distinction
  -- e.g.
  -- `data D a = D a` is not allowed and
  -- `data D a = C a` is allowed
  env <-
    foldM
      ( \env' (A.TypeDefnCtor ctorName ctorArgs) -> do
          when
            (Map.member ctorName env')
            (throwError $ DuplicatedIdentifiers [ctorName])

          let (tvs, ctorTy) = extractMetaVars resultTy ctorArgs
          let diff = filter (`notElem` args) (nub tvs)

          case diff of
            [] -> return $ Map.insert ctorName (A.Forall args ctorTy) env'
            (x : _) -> throwError $ NotInScope x
      )
      kindEnv
      ctors

  let resultDefn =
        T.TypeDefn
          name
          args
          (map (\(A.TypeDefnCtor name' args') -> T.TypeDefnCtor name' args') ctors)
          range

  return (env, resultDefn)
  where
    extractMetaVars baseTy =
      foldr
        ( \argType (ftvs, argTypes) ->
            case argType of
              (A.TMetaVar n _) -> (n : ftvs, A.TVar n (maybeRangeOf n) `typeToType` argTypes)
              ty -> (ftvs, ty `typeToType` argTypes)
        )
        ([], baseTy)
collectDefnToEnv (A.ValDefn name sig expr) = do
  -- NOTE: this part handles recursive functions, consider:
  -- ```
  -- foldr f e Nil = e
  -- foldr f e (Cons x xs) = f x (foldr f e xs)
  -- ```
  -- `foldr : t1` is in the env from `collectSccDefns`
  -- after `infer`-ing the RHS we get substitutions for `t1` and the RHS
  -- `t1` tells us how the function was called in itself as an arbitrary function
  -- (say we change `foldr` in the function body to `g`)
  -- the type of the RHS tells us the actual type of the function body is
  -- and finally we unify the two to get the correct recursive function type
  funcTy <- maybe freshTVar return sig
  _ <- typeToKind funcTy

  (s1, typedExpr) <- typeCheck expr funcTy

  env <- ask
  let ty = case Map.lookup name env of
        Just (A.Forall _ ty') -> applySubst s1 ty'
        Nothing -> error "impossible"

  let funcTy' = applySubst s1 funcTy

  s2 <- lift $ unify funcTy' ty Nothing

  let resultSubst = s2 <> s1
  let resultTy = applySubst s2 funcTy'
  let resultDefn = T.ValDefn name resultTy (applySubstExpr resultSubst typedExpr)

  return (Map.singleton name (A.Forall [] resultTy), resultDefn)

class ToTyped a t | a -> t where
  toTyped :: a -> TIMonad t

instance ToTyped D.Program T.Program where
  toTyped (D.Program defns decls exprs stmts range) = do
    traceM $ "defns: " <> show (pretty (Graph.flattenSCCs defns))
    traceM $ "decls: " <> show (pretty decls)
    traceM $ "exprs: " <> show (pretty exprs)
    traceM $ "stmts: " <> show (pretty stmts)
    env <- ask
    (declEnv, typedDecls) <-
      foldM
        ( \(env', typedDecls') decl -> do
            (declEnv', typedDecl) <- collectDeclToEnv decl
            return (declEnv' <> env', typedDecl : typedDecls')
        )
        (env, [])
        decls
    -- NOTE: since we need to `infer` functions in order to get their types
    -- we also get their typed variants in the same pass
    (defnEnv, typedDefns) <-
      foldM
        ( \(env', typedDefns') defn -> do
            (defnEnv, typedDefns) <- local (const env') (collectSccDefns defn)
            return (defnEnv <> env', typedDefns ++ typedDefns')
        )
        (declEnv, [])
        defns

    let newEnv = defnEnv <> declEnv
    traceM $ show newEnv

    typedExprs <- local (const newEnv) (mapM toTyped exprs)
    typedStmts <- local (const newEnv) (mapM toTyped stmts)

    return $ T.Program typedDefns typedDecls typedExprs typedStmts range

instance ToTyped A.Stmt T.Stmt where
  toTyped (A.Skip range) = return (T.Skip range)
  toTyped (A.Abort range) = return (T.Abort range)
  toTyped (A.Assign names exprs range) = toTypedAssign names exprs range
  toTyped (A.AAssign arr index expr range) = toTypedAAssign arr index expr range
  toTyped (A.Assert expr range) = toTypedAssert expr range
  toTyped (A.LoopInvariant e1 e2 range) = toTypedLoopInvariant e1 e2 range
  toTyped (A.Do gds range) = toTypedDo gds range
  toTyped (A.If gds range) = toTypedIf gds range
  toTyped (A.Spec text range) = T.Spec text range <$> ask
  toTyped (A.Proof t1 t2 range) = return (T.Proof t1 t2 range)
  toTyped (A.Alloc var exprs range) = toTypedAlloc var exprs range
  toTyped (A.HLookup name expr range) = toTypedHLookup name expr range
  toTyped (A.HMutate left right range) = toTypedHMutate left right range
  toTyped (A.Dispose expr range) = toTypedDispose expr range
  toTyped A.Block {} = undefined

toTypedAssign :: [Name] -> [A.Expr] -> Maybe Range -> TIMonad T.Stmt
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
              nameTy <- case Map.lookup name env of
                Just (A.Forall _ ty) -> return ty
                Nothing -> throwError $ NotInScope name

              (exprSubst, typedExpr) <- typeCheck expr nameTy

              return (applySubstExpr exprSubst typedExpr)
          )
          assignments
      return $ T.Assign names typedExprs range

toTypedAAssign :: A.Expr -> A.Expr -> A.Expr -> Maybe Range -> TIMonad T.Stmt
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

toTypedAssert :: A.Expr -> Maybe Range -> TIMonad T.Stmt
toTypedAssert expr range = do
  (s1, exprTy, typedExpr) <- infer expr
  s2 <- lift $ unify exprTy typeBool (maybeRangeOf expr)
  return (T.Assert (applySubstExpr (s2 <> s1) typedExpr) range)

toTypedLoopInvariant :: A.Expr -> A.Expr -> Maybe Range -> TIMonad T.Stmt
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

toTypedDo :: [A.GdCmd] -> Maybe Range -> TIMonad T.Stmt
toTypedDo gds range = do
  typedGds <- mapM toTyped gds
  return (T.Do typedGds range)

toTypedIf :: [A.GdCmd] -> Maybe Range -> TIMonad T.Stmt
toTypedIf gds range = do
  typedGds <- mapM toTyped gds
  return (T.If typedGds range)

toTypedAlloc :: Name -> [A.Expr] -> Maybe Range -> TIMonad T.Stmt
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

toTypedHLookup :: Name -> A.Expr -> Maybe Range -> TIMonad T.Stmt
toTypedHLookup name expr range = do
  env <- ask
  ty <- case Map.lookup name env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ NotInScope name

  s1 <- lift $ unify ty typeInt (maybeRangeOf name)
  (s2, typedExpr) <- local (applySubstEnv s1) (typeCheck expr typeInt)

  return $ T.HLookup name (applySubstExpr (s2 <> s1) typedExpr) range

toTypedHMutate :: A.Expr -> A.Expr -> Maybe Range -> TIMonad T.Stmt
toTypedHMutate left right range = do
  (s1, typedLeft) <- typeCheck left typeInt
  (s2, typedRight) <- local (applySubstEnv s1) (typeCheck right typeInt)
  return $ T.HMutate (applySubstExpr (s2 <> s1) typedLeft) (applySubstExpr (s2 <> s1) typedRight) range

toTypedDispose :: A.Expr -> Maybe Range -> TIMonad T.Stmt
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
    (subst, _ty, typed) <- infer expr
    -- NOTE: invariant doesn't hold during the inference
    -- but `applySubstExpr` traverses the entire subtree
    -- so it is probably really inefficient to do so when
    -- there is likely a few tyvars needed to be substituted
    let typed' = applySubstExpr subst typed

    return typed'

runToTyped :: (ToTyped a t) => a -> Env -> Either TypeError t
runToTyped a env = evalTI (toTyped a) env 0
