{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GCL.SMT.Evaluator(evaluate, evaluateAsString) where

import GCL.SMT.Types
    ( SValue(..),
      Eval(..),
      Convert(convert), valueAsBool, valueAsNum )
import Data.SBV hiding (name)
import qualified Syntax.Typed.Types as T
import qualified Syntax.Common.Types as C
import qualified Syntax.Abstract.Types as A
import qualified Data.Text as Text
import Control.Monad.Trans.State.Lazy (StateT(runStateT), get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad (forM_)

instance Eval T.Expr where
  eval (T.Lit lit _ _) = return $ convert lit
  eval (T.Var name ty _) = evalVariable name ty
  eval (T.Const name ty _) = evalVariable name ty
  eval (T.Op op _) =
    return $ SFunc $ opToFunc op
  eval (T.Chain chain) = eval chain
  eval (T.App l r _) = do
    l' <- eval l
    r' <- eval r
    case l' of
      SFunc func -> return $ func r'
      _ -> error "Unable to apply on literal values"
  eval (T.Subst expr redexes) = do
    expr' <- eval expr
    vars <- get
    forM_ redexes (\(name, substExpr) ->
      case lookup name vars of
        Just var -> do
          substExpr' <- eval substExpr
          lift $ constrain $ var .== substExpr'
        Nothing -> error "Subst var not found"
      )
    lift $ return expr'
  eval expr = error $ "Unsupported expression: " ++ show expr

evalVariable :: C.Name -> A.Type -> StateT [(C.Name, SValue)] Symbolic SValue
evalVariable name@(C.Name t _) ty = do
  vars <- get
  case lookup name vars of
    Just var -> lift $ return var
    Nothing -> case ty of
      A.TBase base _ -> do
        slit <- lift $ freshNamedSLit base (Text.unpack t)
        _ <- put ((name, slit) : vars)
        lift $ return slit
      _ -> error "Unsupported type lookup"


instance Eval T.Chain where
  eval (T.Pure expr) = eval expr
  eval (T.More chain op _ expr) = do
    c <- eval chain
    e <- eval expr
    let f = opToFunc op
    case f c of
      SFunc f' -> return $ f' e
      _ -> error "Unable to apply on literal values"

opToFunc :: C.Op -> (SValue -> SValue)
opToFunc (C.ChainOp op) = case op of
  C.EQ _ -> liftLogicalOp (.==)
  C.NEQ _ -> liftLogicalOp (./=)
  C.NEQU _ -> liftLogicalOp (./=)
  _ -> error $ show op
  where
    liftLogicalOp :: (SValue -> SValue -> SBool) -> (SValue -> SValue)
    liftLogicalOp = curry' . lift' id convert
opToFunc (C.ArithOp op) = case op of
  C.Implies _ -> liftLogicalOp (.=>)
  C.ImpliesU _ -> liftLogicalOp (.=>)
  C.Conj _ -> liftLogicalOp (.&&)
  C.ConjU _ -> liftLogicalOp (.&&)
  C.Disj _ -> liftLogicalOp (.||)
  C.DisjU _ -> liftLogicalOp (.||)
  C.Neg _ -> convert . sNot . valueAsBool
  C.NegU _ -> convert . sNot . valueAsBool

  C.NegNum _ -> convert . negate . valueAsNum
  C.Add _ -> liftArithOp (+)
  C.Sub _ -> liftArithOp (-)
  C.Mul _ -> liftArithOp (*)
  C.Div _ -> liftArithOp sDiv
  C.Mod _ -> liftArithOp sMod
  C.Max _ -> liftArithOp smax
  C.Min _ -> liftArithOp smin
  C.Exp _ -> liftArithOp powUF
  _ -> error $ show op
  where
    liftArithOp :: (SInteger -> SInteger -> SInteger) -> (SValue -> SValue)
    liftArithOp = curry' . lift' valueAsNum convert

    liftLogicalOp :: (SBool -> SBool -> SBool) -> (SValue -> SValue)
    liftLogicalOp = curry' . lift' valueAsBool convert

    powUF :: SInteger -> SInteger -> SInteger
    powUF = uninterpret "pow"
opToFunc (C.TypeOp op) = undefined

lift' :: (SValue -> a) -> (b -> SValue) -> (a -> a -> b) -> SValue -> SValue -> SValue
lift' h' h f x y = h $ f (h' x) (h' y)

curry' :: (SValue -> SValue -> SValue) -> (SValue -> SValue)
curry' f x = SFunc $ \y -> f x y

freshNamedSLit :: A.TBase -> String -> Symbolic SValue
freshNamedSLit baseTy prefix = do
  case baseTy of
    A.TInt -> SNum <$> freeVar
    A.TBool -> SBool <$> freeVar
    A.TChar -> SChar <$> freeVar
  where
    freeVar :: (SymVal a) => Symbolic (SBV a)
    freeVar = free prefix

evaluate :: T.Expr -> IO ()
evaluate expr = do
  result <- sat $ do
    (expr', _) <- runStateT (eval expr) []

    case expr' of
      SBool lit -> do
        constrain $ lit .== sTrue
      _ -> error "Expected to check bool"

  print result

evaluateAsString :: T.Expr -> IO String
evaluateAsString expr = do
  result <- prove $ do
    (expr', _) <- runStateT (eval expr) []

    case expr' of
      SBool lit -> do
        return $ lit .== sTrue
      _ -> error "Expected to check bool"

  return $ show result
