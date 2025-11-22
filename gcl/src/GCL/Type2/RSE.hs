module GCL.Type2.RSE
  ( RSE,
    runRSE,
    evalRSE,
    execRSE,
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
import GCL.Type (TypeError)

type Result = Either TypeError

type RSE r s = RWST r () s Result

runRSE :: RSE r s a -> r -> s -> Result (a, s)
runRSE m r s = (\(a', s', _) -> (a', s')) <$> runRWST m r s

evalRSE :: RSE r s a -> r -> s -> Result a
evalRSE m r s = fst <$> runRSE m r s

execRSE :: RSE r s a -> r -> s -> Result s
execRSE m r s = snd <$> runRSE m r s
