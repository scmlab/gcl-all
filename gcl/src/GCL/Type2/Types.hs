{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.Type2.Types
  ( TIMonad,
    Env,
    TyVar,
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
    -- Utils,
    lift,
    freshTyVar,
    freshTVar,
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
import Syntax.Common.Types (Name(Name))
import qualified Syntax.Abstract.Types as A
import GCL.Common (Counterous(..), Fresh(..))
import GCL.Type (TypeError)

type TyVar = Name

type TmVar = Name

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
evalTI m env c  = evalRSE m env (Inference c)

instance Counterous TIMonad where
  countUp = do n <- gets _counter
               put $ Inference (n + 1)
               return n

instance Fresh TIMonad where
  freshPreS prefix = (pack . (prefix++) . show) <$> countUp

freshTyVar :: TIMonad TyVar
freshTyVar = (\t -> Name t Nothing) <$> freshPreS "t"

freshTVar :: TIMonad A.Type
freshTVar = A.TVar <$> freshTyVar <*> pure Nothing
