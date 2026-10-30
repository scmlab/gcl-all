{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GCL.SMT.Proof (evaluateAsString) where

import Control.Exception (ErrorCall, catch)
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Lazy (evalStateT, get, put)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.SBV
  ( Kind (KBool, KBounded, KChar, KUnbounded),
    SBool,
    SInteger,
    SMTReasonUnknown (UnknownTimeOut),
    SMTResult (Unknown),
    ThmResult (..),
    isConcrete,
  )
import Data.SBV.Dynamic
  ( Quantifier (..),
    SVal,
    svAnd,
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
import qualified Syntax.Typed.Operator as Op
import qualified Syntax.Typed.Types as T

runProof :: Integer -> T.Expr -> IO (Either String ThmResult)
runProof timeoutMs expr =
   catch
        ( runExceptT $ Trans.prove $ do
            Trans.setTimeOut timeoutMs
            result <- evalStateT (buildProof expr) mempty
            case result of
              SVal v -> return $ toBool v
              SFunc _ -> lift $ throwError "Expected a predicate value, not a function"
        )
        (\e -> return $ Left $ show (e :: ErrorCall))
  where
    toBool :: SVal -> SBool
    toBool = SBV

-- | A non-positive millisecond budget skips the solver entirely.
evaluateAsString :: Integer -> [T.Expr] -> T.Expr -> IO String -- XXX: merging `Either` to a `String` is probably a bad idea
evaluateAsString timeoutMs globalProps expr
  | timeoutMs <= 0 = return "Timeout"
  | otherwise = do
      let (expr', stripped) = stripImplies (Op.implies (Op.conjunct globalProps) expr)
      result <- runProof timeoutMs expr'
      return $ case result of
        Left err -> err
        Right (ThmResult (Unknown _ UnknownTimeOut)) -> "Timeout"
        Right proofResult
          -- A counterexample may rely on an assumption that was stripped.
          | stripped && show proofResult /= "Q.E.D." -> "Unknown"
          | otherwise -> show proofResult

-- Strip assumption conjuncts with unsupported quantifiers or symbolic powers,
-- and report whether one was removed so a counterexample is not definite.
stripImplies :: T.Expr -> (T.Expr, Bool)
stripImplies (T.App (T.App op lhs lr) rhs rr)
  | isImplication op =
      let (lhs', stripped) = stripConjuncts lhs
          (rhs', stripped') = stripImplies rhs
       in (T.App (T.App op lhs' lr) rhs' rr, stripped || stripped')
stripImplies expr = (expr, False)

stripConjuncts :: T.Expr -> (T.Expr, Bool)
stripConjuncts (T.App (T.App op lhs lr) rhs rr)
  | isConjunction op =
      let (lhs', stripped) = stripConjuncts lhs
          (rhs', stripped') = stripConjuncts rhs
       in (T.App (T.App op lhs' lr) rhs' rr, stripped || stripped')
stripConjuncts expr = stripExpr expr

stripExpr :: T.Expr -> (T.Expr, Bool)
stripExpr expr
  | hasUnsupportedQuantifier expr = (Op.true, True)
stripExpr expr@(T.Chain (T.More {})) =
  let symbolic = hasSymbolicExponent expr
   in (if symbolic then Op.true else expr, symbolic)
stripExpr expr = (expr, False)

hasUnsupportedQuantifier :: T.Expr -> Bool
hasUnsupportedQuantifier = anyExpr unsupportedQuantifier
  where
    unsupportedQuantifier (T.Quant op _ _ _ _) = isNothing (quantifierKind op)
    unsupportedQuantifier _ = False

hasSymbolicExponent :: T.Expr -> Bool
hasSymbolicExponent = anyExpr symbolicPower
  where
    symbolicPower (T.App (T.App (T.Op (C.ArithOp (C.Exp _)) _) _ _) exponentExpr _) =
      hasSymbolic exponentExpr
    symbolicPower _ = False

hasSymbolic :: T.Expr -> Bool
hasSymbolic = anyExpr isVariable
  where
    isVariable T.Var {} = True
    isVariable T.Const {} = True
    isVariable _ = False

anyExpr :: (T.Expr -> Bool) -> T.Expr -> Bool
anyExpr predicate expr =
  predicate expr || case expr of
    T.App function argument _ -> visit function || visit argument
    T.Chain chain -> visitChain chain
    T.Lam _ _ body _ -> visit body
    T.Tuple elements -> any visit elements
    T.OutT _ inner -> visit inner
    T.Quant op _ condition body _ -> any visit [op, condition, body]
    T.ArrIdx array index _ -> visit array || visit index
    T.ArrUpd array index value _ -> any visit [array, index, value]
    T.Case value clauses _ -> visit value || any (\(T.CaseClause _ body) -> visit body) clauses
    T.Subst body redexes -> visit body || any (visit . snd) redexes
    _ -> False
  where
    visit = anyExpr predicate
    visitChain (T.Pure operand) = visit operand
    visitChain (T.More chain _ _ operand) = visitChain chain || visit operand

isImplication, isConjunction :: T.Expr -> Bool
isImplication (T.Op (C.ArithOp (C.Implies _)) _) = True
isImplication (T.Op (C.ArithOp (C.ImpliesU _)) _) = True
isImplication _ = False
isConjunction (T.Op (C.ArithOp (C.Conj _)) _) = True
isConjunction (T.Op (C.ArithOp (C.ConjU _)) _) = True
isConjunction _ = False

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
    quant <- case quantifierKind opExpr of
      Just supported -> return supported
      Nothing -> throwError "Unknown.\n  Unsupported quantifier."
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
        mkUninterpretedFunc (Text.unpack t) ty
      _ -> throwError "Unsupported type lookup"

mkUninterpretedFunc :: String -> A.Type -> BuildState SValue
mkUninterpretedFunc name = build []
  where
    build :: [SVal] -> A.Type -> BuildState SValue
    build collected (A.TApp (A.TApp (A.TOp (C.Arrow _)) _argTy _) retTy _) =
      return $ SFunc $ \arg -> do
        arg' <- asVal arg
        build (arg' : collected) retTy
    build collected (A.TBase base _) =
      return $ SVal $ svUninterpreted (baseTyToKind base) name (UINone True) (reverse collected)
    build _ ty =
      throwError $ "Unsupported uninterpreted function type layer: " ++ show ty

instance ProofBuilder T.Chain where
  buildProof chain = do
    (lastOperand, comparisons) <- buildChain chain
    return $ maybe lastOperand SVal comparisons

-- Keep the last operand separate from the comparisons already made. The next
-- comparison uses that operand, rather than the Boolean result of the chain.
buildChain :: T.Chain -> BuildState (SValue, Maybe SVal)
buildChain (T.Pure expr) = do
  operand <- buildProof expr
  return (operand, Nothing)
buildChain (T.More chain op _ expr) = do
  (prevOperand, cmp) <- buildChain chain
  operand <- buildProof expr
  comparisonFunction <- opToFunc op prevOperand
  comparison <- case comparisonFunction of
    SFunc f -> f operand >>= asVal
    _ -> throwError "Unable to apply comparison operator"
  let allComparisons = maybe comparison (`svAnd` comparison) cmp
  return (operand, Just allComparisons)

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
          y' <- asVal y
          if isConcrete (SBV y' :: SInteger)
            then return $ convert $ svExp x' (svFromIntegral (KBounded False 32) y')
            else throwError "Unknown"
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

quantifierKind :: T.Expr -> Maybe Quantifier
quantifierKind (T.Op (C.ArithOp (C.Conj _)) _) = Just ALL
quantifierKind (T.Op (C.ArithOp (C.ConjU _)) _) = Just ALL
quantifierKind (T.Op (C.ArithOp (C.Disj _)) _) = Just EX
quantifierKind (T.Op (C.ArithOp (C.DisjU _)) _) = Just EX
quantifierKind _ = Nothing -- FIXME(ChAoS): Implement all quantifiers.
