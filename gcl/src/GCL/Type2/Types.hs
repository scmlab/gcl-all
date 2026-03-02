{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GCL.Type2.Types
  ( TIMonad,
    Env,
    TyVar,
    Subst,
    -- TIMonad
    evalTI,
    -- Reader
    reader,
    ask,
    asks,
    local,
    -- State
    state,
    get,
    gets,
    put,
    modify,
    -- Error
    Result,
    throwError,
    -- Utils
    lift,
    freshTyVar,
    freshTVar,
    -- Useful Constants
    typeToType,
    typeInt,
    typeBool,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.RWS
  ( MonadReader (ask, local, reader),
    MonadState (get, put, state),
    MonadTrans (lift),
    RWST,
    asks,
    gets,
    modify,
    runRWST,
  )
import Data.Map (Map)
import Data.Text (pack)
import GCL.Common (Counterous (..), Fresh (..))
import GCL.Type (TypeError)
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name (Name), TypeOp (..))

type TyVar = Name

type TmVar = Name

type Subst = Map TyVar A.Type

type Env = Map TmVar A.Scheme

type Result = Either TypeError

type RSE r s = RWST r () s Result

runRSE :: RSE r s a -> r -> s -> Result (a, s)
runRSE m r s = (\(a', s', _) -> (a', s')) <$> runRWST m r s

evalRSE :: RSE r s a -> r -> s -> Result a
evalRSE m r s = fst <$> runRSE m r s

execRSE :: RSE r s a -> r -> s -> Result s
execRSE m r s = snd <$> runRSE m r s

newtype Inference = Inference
  { _counter :: Int
  }

type TIMonad = RSE Env Inference

evalTI :: TIMonad a -> Env -> Int -> Result a
evalTI m env c = evalRSE m env (Inference c)

instance Counterous TIMonad where
  countUp = do
    n <- gets _counter
    put $ Inference (n + 1)
    return n

instance Fresh TIMonad where
  freshPreS prefix = (pack . (prefix ++) . show) <$> countUp

freshTyVar :: TIMonad TyVar
freshTyVar = (\t -> Name t Nothing) <$> freshPreS "t"

freshTVar :: TIMonad A.Type
freshTVar = A.TVar <$> freshTyVar <*> pure Nothing

infixr 1 `typeToType`

-- | construct a type of `a -> b`
typeToType :: A.Type -> A.Type -> A.Type
typeToType t1 t2 = A.TApp (A.TApp (A.TOp (Arrow Nothing)) t1 Nothing) t2 Nothing

-- XXX: is including `Loc` relevant here?
typeInt :: A.Type
typeInt = A.TBase A.TInt Nothing

typeBool :: A.Type
typeBool = A.TBase A.TBool Nothing
