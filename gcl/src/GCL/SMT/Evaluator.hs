{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GCL.SMT.Evaluator(evaluateAsString) where

import GCL.SMT.Types
    ( SValue(..),
      ProofBuilder(..),
      Convert(convert), valueAsBool, valueAsNum, BuildState, ExceptableSymbolic)
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

instance ProofBuilder T.Expr where
  buildProof (T.Lit lit _ _) = return $ convert lit
  buildProof (T.Var name ty _) = evalVariable name ty
  buildProof (T.Const name ty _) = evalVariable name ty
  buildProof (T.Op op _) =
    return $ SFunc $ opToFunc op
  buildProof (T.Chain chain) = buildProof chain
  buildProof (T.App l r _) = do
    l' <- buildProof l
    r' <- buildProof r
    case l' of
      SFunc func -> return $ func r'
      _ -> error "Unable to apply on literal values"
  buildProof (T.Subst expr redexes) = do
    expr' <- buildProof expr
    vars <- get
    forM_ redexes (\(name, substExpr) ->
      case Map.lookup name vars of
        Just var -> do
          substExpr' <- buildProof substExpr
          lift $ constrain $ var .== substExpr'
        Nothing -> error "Subst var not found"
      )
    return expr'
  buildProof expr = error $ "Unsupported expression: " ++ show expr

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
      _ -> error "Unsupported type lookup"


instance ProofBuilder T.Chain where
  buildProof (T.Pure expr) = buildProof expr
  buildProof (T.More chain op _ expr) = do
    c <- buildProof chain
    e <- buildProof expr
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

freshNamedSLit :: A.TBase -> String -> ExceptableSymbolic SValue
freshNamedSLit baseTy prefix = do
  case baseTy of
    A.TInt -> SNum <$> freeVar
    A.TBool -> SBool <$> freeVar
    A.TChar -> SChar <$> freeVar
  where
    freeVar :: (SymVal a) => ExceptableSymbolic (SBV a)
    freeVar = Trans.free prefix

buildExprProof :: T.Expr -> ExceptableSymbolic SBool
buildExprProof expr = do
  result <- buildProof expr
  case result of
    SBool predicate -> return predicate
    _ -> throwError ""

runProof :: T.Expr -> IO (Either String ThmResult)
runProof expr = runExceptT (Trans.prove $ evalStateT (buildExprProof expr) mempty)

evaluateAsString :: T.Expr -> IO String
evaluateAsString expr = do
  result <- runProof expr
  return $ show result
