{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GCL.SMT.Evaluator(evaluate, evaluateAsString) where

import GCL.SMT.Types
    ( SValue(..),
      SLit(bool, num, SLit, tag),
      Eval(..),
      Convert(convert) )
import Data.SBV
    ( SInteger,
      SBool,
      SymVal(literal),
      EqSymbolic((.==), (./=)),
      Symbolic,
      constrain,
      sat,
      sTrue,
      free,
      SDivisible(sDiv), (.=>), (.&&), (.||), prove)
import qualified Syntax.Typed.Types as T
import qualified Syntax.Common.Types as C
import qualified Syntax.Abstract.Types as A
import qualified Data.Text as Text
import Control.Monad.Trans.State.Lazy (StateT(runStateT), get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad (forM_)

instance Eval T.Expr where
  eval (T.Lit lit _ _) = return $ SLiteral $ convert lit
  eval (T.Var name@(C.Name t _) ty _) = do
    vars <- get
    case lookup name vars of
      Just var -> lift $ return var
      Nothing -> case ty of
        A.TBase base _ -> do
          slit <- lift $ SLiteral <$> freshNamedSLit base (Text.unpack t)
          _ <- put ((name, slit) : vars)
          lift $ return slit
        _ -> error "Unsupported type lookup"
  eval (T.Const name@(C.Name t _) ty _) = do
    vars <- get
    case lookup name vars of
      Just var -> lift $ return var
      Nothing -> case ty of
        A.TBase base _ -> do
          slit <- lift $ SLiteral <$> freshNamedSLit base (Text.unpack t)
          _ <- put ((name, slit) : vars)
          lift $ return slit
        _ -> error "Unsupported type lookup"
  eval (T.Op op _) =
    return $ SFunc $ return . opToFunc op
  eval (T.Chain chain) = eval chain
  eval (T.App l r _) = do
    l' <- eval l
    r' <- eval r
    case l' of
      SLiteral _ -> error "Unable to apply on literal"
      SFunc func -> lift $ func r'
  eval (T.Subst expr redexes) = do
    expr' <- eval expr
    vars <- get
    forM_ redexes (\(name, substExpr) ->
      case lookup name vars of
        Just var -> do
          substExpr' <- eval substExpr
          case (var, substExpr') of
            (SLiteral exprLit, SLiteral substExprLit) -> do
              lift $ constrain $ exprLit .== substExprLit
            _ -> error "Unable to constrain symbolic function"
        Nothing -> error "Subst var not found"
      )
    lift $ return expr'
  eval expr = error $ "Unsupported expression: " ++ show expr

instance Eval T.Chain where
  eval (T.Pure expr) = eval expr
  eval (T.More chain op _ expr) = do
    c <- eval chain
    e <- eval expr
    let f = opToFunc op
    case f c of
      SLiteral _ -> error ""
      SFunc f' -> lift $ f' e

opToFunc :: C.Op -> (SValue -> SValue)
opToFunc (C.ChainOp op) = case op of
  C.EQ _ -> liftLogicalOp (.==)
  C.NEQ _ -> liftLogicalOp (./=)
  C.NEQU _ -> liftLogicalOp (./=)
  _ -> error $ show op
  where
    liftLogicalOp :: (SLit -> SLit -> SBool) -> (SValue -> SValue)
    liftLogicalOp = curry' . lift' unLit (SLiteral . convert)
opToFunc (C.ArithOp op) = case op of
  C.Implies _ -> liftLogicalOp (.=>)
  C.ImpliesU _ -> liftLogicalOp (.=>)

  C.Conj _ -> liftLogicalOp (.&&)
  C.ConjU _ -> liftLogicalOp (.&&)
  C.Disj _ -> liftLogicalOp (.||)

  C.NegNum _ -> SLiteral . convert . negate . num . unLit
  C.Add _ -> liftArithOp (+)
  C.Sub _ -> liftArithOp (-)
  C.Mul _ -> liftArithOp (*)
  C.Div _ -> liftArithOp sDiv
  _ -> error $ show op
  where
    liftArithOp :: (SInteger -> SInteger -> SInteger) -> (SValue -> SValue)
    liftArithOp = curry' . lift' (num . unLit) (SLiteral . convert)

    liftLogicalOp :: (SBool -> SBool -> SBool) -> (SValue -> SValue)
    liftLogicalOp = curry' . lift' (bool . unLit) (SLiteral . convert)
opToFunc (C.TypeOp op) = undefined

lift' :: (SValue -> a) -> (b -> SValue) -> (a -> a -> b) -> SValue -> SValue -> SValue
lift' h' h f x y = h $ f (h' x) (h' y)

curry' :: (SValue -> SValue -> SValue) -> (SValue -> SValue)
curry' f x = SFunc $ \y -> return $ f x y

unLit :: SValue -> SLit
unLit (SLiteral l) = l
unLit _ = error "Expected an literal here, but got high-order function"

freshNamedSLit :: A.TBase -> String -> Symbolic SLit
freshNamedSLit baseTy prefix = do
  let t = case baseTy of
        A.TInt -> literal 0
        A.TBool -> literal 1
        A.TChar -> literal 2
  n <- free (prefix ++ "_num")
  b <- free (prefix ++ "_bool")
  c <- free (prefix ++ "_char")
  return $ SLit t n b c

evaluate :: T.Expr -> IO ()
evaluate expr = do
  result <- sat $ do
    (expr', _) <- runStateT (eval expr) []

    case expr' of
      SLiteral lit -> do
        constrain $ tag lit .== 1
        constrain $ bool lit .== sTrue
      SFunc _ -> do
        error ""

  print result

evaluateAsString :: T.Expr -> IO String
evaluateAsString expr = do
  result <- prove $ do
    (expr', _) <- runStateT (eval expr) []

    case expr' of
      SLiteral lit -> do
        constrain $ tag lit .== 1
        return $ bool lit .== sTrue
      SFunc _ -> do
        error ""

  return $ show result
