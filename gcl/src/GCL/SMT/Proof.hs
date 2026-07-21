{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GCL.SMT.Proof (evaluateAsString) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Lazy (evalStateT, get, put)
import qualified Data.Map as Map
import Data.SBV
  ( Kind (KBool, KBounded, KChar, KUnbounded),
    SBool,
    ThmResult,
  )
import Data.SBV.Dynamic
  ( SVal,
    svAnd,
    svAsInteger,
    svDivide,
    svEqual,
    svExp,
    svFromIntegral,
    svGreaterEq,
    svGreaterThan,
    svIte,
    svLessEq,
    svLessThan,
    svMinus,
    svNewVar,
    svNot,
    svNotEqual,
    svOr,
    svPlus,
    svRem,
    svTimes,
    svUNeg,
  )
import Data.SBV.Internals (SBV (..))
import qualified Data.SBV.Trans as Trans
import qualified Data.Text as Text
import GCL.SMT.Types
  ( BuildState,
    Convert (convert),
    ProofBuilder (..),
    SValue (..),
  )
import qualified Syntax.Abstract.Types as A
import qualified Syntax.Common.Types as C
import qualified Syntax.Typed.Types as T

runProof :: T.Expr -> IO (Either String ThmResult)
runProof expr = runExceptT $ Trans.prove $ do
  result <- evalStateT (buildProof expr) mempty
  case result of
    SVal v -> return $ toBool v
    SFunc _ -> lift $ throwError "Expression did not fully reduce to a value; cannot prove a function"
  where
    toBool :: SVal -> SBool
    toBool = SBV

evaluateAsString :: T.Expr -> IO String
evaluateAsString expr = do
  result <- runProof expr
  return $ case result of
    Left err -> err
    Right result' -> show result'

instance ProofBuilder T.Expr where
  buildProof (T.Lit lit _ _) = return $ convert lit
  buildProof (T.Var name ty _) = genVariable name ty
  buildProof (T.Const name ty _) = genVariable name ty
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
    -- Proof building for Subst is as follow:
    -- The requirement for VarsMap (in the state) should be equivalant
    -- for before and after state of Subst proof building.
    --
    -- The actual progress is as follow:
    -- Retreive the state first, and builds redex expressions and put into
    -- state by the accompanied name, then builds the substituted expression.
    -- Finally, restore the state from the state retreived at the beginning of
    -- proof building, and return the expression.
    originalVars <- get
    substs <-
      mapM
        ( \(name, substExpr) -> do
            substExpr' <- buildProof substExpr
            return (name, substExpr')
        )
        redexes
    vars <- get
    put (foldr (\(name, v) m -> Map.insert name v m) vars substs)
    expr' <- buildProof expr
    put originalVars
    return expr'
  buildProof expr = throwError $ "Unsupported expression: " ++ show expr

genVariable :: C.Name -> A.Type -> BuildState SValue
genVariable name@(C.Name t _) ty = do
  vars <- get
  case Map.lookup name vars of
    Just var -> lift $ return var
    Nothing -> case ty of
      A.TBase base _ -> do
        lit <- freshNamedSLit base (Text.unpack t)
        put (Map.insert name lit vars)
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
  C.EQ _ -> genFunc svEqual
  C.NEQ _ -> genFunc svNotEqual
  C.NEQU _ -> genFunc svNotEqual
  C.LTE _ -> genFunc svLessEq
  C.LTEU _ -> genFunc svLessEq
  C.GTE _ -> genFunc svGreaterEq
  C.GTEU _ -> genFunc svGreaterEq
  C.LT _ -> genFunc svLessThan
  C.GT _ -> genFunc svGreaterThan
  _ -> return $ throwError $ show op
opToFunc (C.ArithOp op) = case op of
  C.Implies _ -> genFunc (\x y -> svNot x `svOr` y)
  C.ImpliesU _ -> genFunc (\x y -> svNot x `svOr` y)
  C.Conj _ -> genFunc svAnd
  C.ConjU _ -> genFunc svAnd
  C.Disj _ -> genFunc svOr
  C.DisjU _ -> genFunc svOr
  C.Neg _ -> \x -> do
    b <- asVal x
    return $ SVal $ svNot b
  C.NegU _ -> \x -> do
    b <- asVal x
    return $ SVal $ svNot b
  C.NegNum _ -> \x -> do
    n <- asVal x
    return $ SVal $ svUNeg n
  C.Add _ -> genFunc svPlus
  C.Sub _ -> genFunc svMinus
  C.Mul _ -> genFunc svTimes
  C.Div _ -> genFunc svDivide
  C.Mod _ -> genFunc svRem
  C.Max _ -> genFunc (\x y -> svIte (x `svGreaterThan` y) x y)
  C.Min _ -> genFunc (\x y -> svIte (x `svLessThan` y) x y)
  C.Exp _ ->
    curry'
      ( \x y -> do
          x' <- asVal x
          y' <- svFromIntegral (KBounded False 32) <$> asVal y
          case svAsInteger y' of
            Just _ -> return $ SVal $ svExp x' y'
            Nothing -> throwError "Unsupported exponential operation: RHS must be concrete value"
      )
  _ -> return $ throwError $ show op
opToFunc (C.TypeOp _) = return $ throwError "Type Op is not yet implemented"

genFunc :: (SVal -> SVal -> SVal) -> (SValue -> BuildState SValue)
genFunc = curry' . lift'

lift' :: (SVal -> SVal -> SVal) -> SValue -> SValue -> BuildState SValue
lift' f x y = do
  a1 <- asVal x
  a2 <- asVal y
  return $ SVal $ f a1 a2

curry' :: (SValue -> SValue -> BuildState SValue) -> (SValue -> BuildState SValue)
curry' f x = return $ SFunc $ \y -> f x y

freshNamedSLit :: A.TBase -> String -> BuildState SValue
freshNamedSLit baseTy prefix = SVal <$> svNewVar kind prefix
  where
    kind :: Kind
    kind = case baseTy of
      A.TInt -> KUnbounded
      A.TBool -> KBool
      A.TChar -> KChar

asVal :: SValue -> BuildState SVal
asVal (SVal v) = return v
asVal _ = throwError "Not a sval"
