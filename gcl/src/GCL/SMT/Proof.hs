{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GCL.SMT.Proof(evaluateAsString) where

import GCL.SMT.Types
    ( SValue(..),
      ProofBuilder(..),
      Convert(convert), BuildState, ExceptableSymbolic)
import Data.SBV hiding (name)
import qualified Syntax.Typed.Types as T
import qualified Syntax.Common.Types as C
import qualified Syntax.Abstract.Types as A
import qualified Data.Text as Text
import Control.Monad.Trans.State.Lazy (get, put, evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad (forM_)
import qualified Data.SBV.Trans as Trans
import Control.Monad.Except (MonadError(..))
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Map as Map

buildExprProof :: T.Expr -> ExceptableSymbolic SBool
buildExprProof expr = do
  result <- buildProof expr
  case result of
    SBool predicate -> return predicate
    _ -> throwError "Expected proof to have boolean type"

runProof :: T.Expr -> IO (Either String ThmResult)
runProof expr = runExceptT (Trans.prove $ evalStateT (buildExprProof expr) mempty)

evaluateAsString :: T.Expr -> IO String
evaluateAsString expr = do
  result <- runProof expr
  return $ case result of
    Left err -> err
    Right result' -> show result'

instance ProofBuilder T.Expr where
  buildProof (T.Lit lit _ _) = return $ convert lit
  buildProof (T.Var name ty _) = evalVariable name ty
  buildProof (T.Const name ty _) = evalVariable name ty
  buildProof (T.Op op _) = do
    return $ SFunc $ opToFunc op
  buildProof (T.Chain chain) = buildProof chain
  buildProof (T.App l r _) = do
    l' <- buildProof l
    r' <- buildProof r
    case l' of
      SFunc func -> func r'
      _ -> throwError "Unable to apply on literal values"
  buildProof (T.Subst expr redexes) = do
    expr' <- buildProof expr
    vars <- get
    forM_ redexes (\(name, substExpr) ->
      case Map.lookup name vars of
        Just var -> do
          substExpr' <- buildProof substExpr
          lift $ constrain $ var .== substExpr'
        Nothing -> throwError "Subst var not found"
      )
    return expr'
  buildProof expr = throwError $ "Unsupported expression: " ++ show expr

evalVariable :: C.Name -> A.Type -> BuildState SValue
evalVariable name@(C.Name t _) ty = do
  vars <- get
  case Map.lookup name vars of
    Just var -> lift $ return var
    Nothing -> case ty of
      A.TBase base _ -> do
        lit <- freshNamedSLit base (Text.unpack t)
        _ <- put (Map.insert name lit vars)
        lift $ return lit
      _ -> throwError "Unsupported type lookup"


instance ProofBuilder T.Chain where
  buildProof (T.Pure expr) = buildProof expr
  buildProof (T.More chain op _ expr) = do
    c <- buildProof chain
    e <- buildProof expr
    r <- opToFunc op c
    case r of
      SFunc f' -> f' e
      _ -> throwError "Unable to apply on literal values"

opToFunc :: C.Op -> (SValue -> BuildState SValue)
opToFunc (C.ChainOp op) = case op of
  C.EQ _ -> liftLogicalOp (.==)
  C.NEQ _ -> liftLogicalOp (./=)
  C.NEQU _ -> liftLogicalOp (./=)
  C.LTE _ -> liftRelOp (.<=)
  C.LTEU _ -> liftRelOp (.<=)
  C.GTE _ -> liftRelOp (.>=)
  C.GTEU _ -> liftRelOp (.>=)
  C.LT _ -> liftRelOp (.>)
  C.GT _ -> liftRelOp (.<)
  _ -> return $ throwError $ show op
  where
    liftRelOp :: (SInteger -> SInteger -> SBool) -> (SValue -> BuildState SValue)
    liftRelOp = curry' . lift' valueAsNum convert

    liftLogicalOp :: (SValue -> SValue -> SBool) -> (SValue -> BuildState SValue)
    liftLogicalOp = curry' . lift' return convert
opToFunc (C.ArithOp op) = case op of
  C.Implies _ -> liftLogicalOp (.=>)
  C.ImpliesU _ -> liftLogicalOp (.=>)
  C.Conj _ -> liftLogicalOp (.&&)
  C.ConjU _ -> liftLogicalOp (.&&)
  C.Disj _ -> liftLogicalOp (.||)
  C.DisjU _ -> liftLogicalOp (.||)
  C.Neg _ -> \x -> do
    b <- valueAsBool x
    return $ convert (sNot b)
  C.NegU _ -> \x -> do
    b <- valueAsBool x
    return $ convert (sNot b)

  C.NegNum _ -> \x -> do
    n <- valueAsNum x
    return $ convert (negate n)
  C.Add _ -> liftArithOp (+)
  C.Sub _ -> liftArithOp (-)
  C.Mul _ -> liftArithOp (*)
  C.Div _ -> liftArithOp sDiv
  C.Mod _ -> liftArithOp sMod
  C.Max _ -> liftArithOp smax
  C.Min _ -> liftArithOp smin
  C.Exp _ -> curry' (\x y -> do
    x' <- valueAsNum x
    y' <- sFromIntegral <$> valueAsNum y :: BuildState SWord32
    if isConcrete y' then
      return $ convert $ x' .^ y'
    else
      throwError "Unsupported exponential operation: RHS must be concrete value"
    )
  _ -> return $ throwError $ show op
  where
    liftArithOp :: (SInteger -> SInteger -> SInteger) -> (SValue -> BuildState SValue)
    liftArithOp = curry' . lift' valueAsNum convert

    liftLogicalOp :: (SBool -> SBool -> SBool) -> (SValue -> BuildState SValue)
    liftLogicalOp = curry' . lift' valueAsBool convert
opToFunc (C.TypeOp op) = return $ throwError "Type Op is not yet implemented"

lift' :: (SValue -> BuildState a) -> (b -> SValue) -> (a -> a -> b) -> SValue -> SValue -> BuildState SValue
lift' h' h f x y = do
  a1 <- h' x
  a2 <- h' y
  return $ h $ f a1 a2

curry' :: (SValue -> SValue -> BuildState SValue) -> (SValue -> BuildState SValue)
curry' f x = return $ SFunc $ \y -> f x y

freshNamedSLit :: A.TBase -> String -> ExceptableSymbolic SValue
freshNamedSLit baseTy prefix = do
  case baseTy of
    A.TInt -> SNum <$> freeVar
    A.TBool -> SBool <$> freeVar
    A.TChar -> SChar <$> freeVar
  where
    freeVar :: (SymVal a) => ExceptableSymbolic (SBV a)
    freeVar = Trans.free prefix

valueAsBool :: SValue -> BuildState SBool
valueAsBool (SBool b) = return b
valueAsBool _ = throwError "Not a bool"

valueAsNum :: SValue -> BuildState SInteger
valueAsNum (SNum i) = return i
valueAsNum _ = throwError "Not a num"
