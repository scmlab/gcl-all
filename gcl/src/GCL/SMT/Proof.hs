{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GCL.SMT.Proof (evaluateAsString) where

import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (liftIO)
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
  ( Quantifier (..),
    SVal,
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
    svMkSymVar,
    svNot,
    svNotEqual,
    svOr,
    svPlus,
    svRem,
    svTimes,
    svUNeg,
    svUninterpreted,
  )
import Data.SBV.Internals (SBV (..), UICodeKind (UINone), VarContext (NonQueryVar))
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
    SFunc _ -> lift $ throwError "Expected a predicate value, not a function"
  where
    toBool :: SVal -> SBool
    toBool = SBV

evaluateAsString :: T.Expr -> IO String
evaluateAsString expr = either id show <$> runProof expr

instance ProofBuilder T.Expr where
  buildProof (T.Lit lit _ _) = return $ convert lit
  buildProof (T.Var name ty _) = genVariable ALL name ty
  buildProof (T.Const name ty _) = genVariable ALL name ty
  buildProof (T.Op op _) = do
    return $ SFunc $ opToFunc op
  buildProof (T.Chain chain) = buildProof chain
  buildProof (T.App l r _) = do
    l' <- buildProof l
    r' <- buildProof r
    case l' of
      SFunc func -> func r'
      _ -> throwError "Unable to apply on literal values"
  buildProof (T.Quant opExpr bounds condExpr expr _) = do
    quant <- case opExpr of
      T.Op (C.ArithOp (C.Conj _)) _ -> return ALL
      T.Op (C.ArithOp (C.ConjU _)) _ -> return ALL
      T.Op (C.ArithOp (C.Disj _)) _ -> return EX
      T.Op (C.ArithOp (C.DisjU _)) _ -> return EX
      op@(T.Op _ _) -> throwError $ "Not a valid quantifier operator: " <> show op
      expr' -> throwError $ "Not a valid quantifier operator expression: " <> show expr'
    originalVars <- get
    forM_ bounds (uncurry (genVariable quant))
    condExpr' <- buildProof condExpr
    expr' <- buildProof expr
    put originalVars
    case (condExpr', expr') of
      (SVal condVal, SVal exprVal) -> return $ convert $ case quant of
        ALL -> svNot condVal `svOr` exprVal
        EX -> condVal `svAnd` exprVal
      _ -> throwError $ "Not a valid quantifier expression"
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

genVariable :: Quantifier -> C.Name -> A.Type -> BuildState SValue
genVariable quant name@(C.Name t _) ty = do
  vars <- get
  case Map.lookup name vars of
    Just var -> lift $ return var
    Nothing -> case ty of
      A.TBase base _ -> do
        lit <- mkFreshSymVar quant base (Text.unpack t)
        put (Map.insert name lit vars)
        lift $ return lit
      A.TApp (A.TApp (A.TOp (C.Arrow _)) _ _) _ _ -> do
        lift $ return $ mkUninterpretedFunc (Text.unpack t) ty
      _ -> throwError "Unsupported type lookup"

mkUninterpretedFunc :: String -> A.Type -> SValue
mkUninterpretedFunc name = build []
  where
    build :: [SVal] -> A.Type -> SValue
    build collected (A.TApp (A.TApp (A.TOp (C.Arrow _)) _argTy _) retTy _) =
      SFunc $ \arg -> do
        arg' <- asVal arg
        return $ build (arg' : collected) retTy
    build collected (A.TBase base _) =
      SVal $ svUninterpreted (baseTyToKind base) name (UINone True) (reverse collected)
    build _ ty =
      error $ "Unsupported uninterpreted function type layer: " ++ show ty

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
  C.EQ _ -> genBinaryFunc svEqual
  C.NEQ _ -> genBinaryFunc svNotEqual
  C.NEQU _ -> genBinaryFunc svNotEqual
  C.LTE _ -> genBinaryFunc svLessEq
  C.LTEU _ -> genBinaryFunc svLessEq
  C.GTE _ -> genBinaryFunc svGreaterEq
  C.GTEU _ -> genBinaryFunc svGreaterEq
  C.LT _ -> genBinaryFunc svLessThan
  C.GT _ -> genBinaryFunc svGreaterThan
  _ -> return $ throwError $ "Unsupported chain op: " <> show op
opToFunc (C.ArithOp op) = case op of
  -- FIXME(ChAoS): Somehow svImplies is not visible?
  C.Implies _ -> genBinaryFunc (\x y -> svNot x `svOr` y)
  C.ImpliesU _ -> genBinaryFunc (\x y -> svNot x `svOr` y)
  C.Conj _ -> genBinaryFunc svAnd
  C.ConjU _ -> genBinaryFunc svAnd
  C.Disj _ -> genBinaryFunc svOr
  C.DisjU _ -> genBinaryFunc svOr
  C.Neg _ -> genUnaryFunc svNot
  C.NegU _ -> genUnaryFunc svNot
  C.NegNum _ -> genUnaryFunc svUNeg
  C.Add _ -> genBinaryFunc svPlus
  C.Sub _ -> genBinaryFunc svMinus
  C.Mul _ -> genBinaryFunc svTimes
  C.Div _ -> genBinaryFunc svDivide
  C.Mod _ -> genBinaryFunc svRem
  C.Max _ -> genBinaryFunc (\x y -> svIte (x `svGreaterThan` y) x y)
  C.Min _ -> genBinaryFunc (\x y -> svIte (x `svLessThan` y) x y)
  C.Exp _ ->
    curry'
      ( \x y -> do
          x' <- asVal x
          y' <- svFromIntegral (KBounded False 32) <$> asVal y
          case svAsInteger y' of
            Just _ -> return $ convert $ svExp x' y'
            Nothing -> throwError "Unsupported exponential operation: RHS must be concrete value"
      )
  _ -> return $ throwError $ "Unsupported arith op: " <> show op
opToFunc (C.TypeOp _) = return $ throwError "Type Op is not yet implemented"

genUnaryFunc :: (SVal -> SVal) -> (SValue -> BuildState SValue)
genUnaryFunc = lift1'

genBinaryFunc :: (SVal -> SVal -> SVal) -> (SValue -> BuildState SValue)
genBinaryFunc = curry' . lift2'

lift1' :: (SVal -> SVal) -> SValue -> BuildState SValue
lift1' f x = do
  x' <- asVal x
  return $ convert $ f x'

lift2' :: (SVal -> SVal -> SVal) -> SValue -> SValue -> BuildState SValue
lift2' f x y = do
  x' <- asVal x
  y' <- asVal y
  return $ convert $ f x' y'

curry' :: (SValue -> SValue -> BuildState SValue) -> (SValue -> BuildState SValue)
curry' f x = return $ SFunc $ \y -> f x y

mkFreshSymVar :: Quantifier -> A.TBase -> String -> BuildState SValue
mkFreshSymVar quant baseTy name = convert <$> var
  where
    var :: BuildState SVal
    var = Trans.symbolicEnv >>= liftIO . svMkSymVar (NonQueryVar $ Just quant) kind (Just name)

    kind :: Kind
    kind = case baseTy of
      A.TInt -> KUnbounded
      A.TBool -> KBool
      A.TChar -> KChar

asVal :: SValue -> BuildState SVal
asVal (SVal v) = return v
asVal _ = throwError "Not a sval"

baseTyToKind :: A.TBase -> Kind
baseTyToKind baseTy = case baseTy of
  A.TInt -> KUnbounded
  A.TBool -> KBool
  A.TChar -> KChar
