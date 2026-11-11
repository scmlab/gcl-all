{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.Type2.Types
  ( TIMonad,
    Env,
    TyVar,
    Subst,
    Inference,
    mkInference,
    -- TIMonad
    evalTI,
    runTI,
    -- Reader
    reader,
    ask,
    asks,
    askEnv,
    askPEnv,
    local,
    localEnv,
    -- State
    state,
    get,
    gets,
    put,
    modify,
    -- Error
    TypeError (..),
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
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Text (pack)
import GCL.Common (Counterous (..), Fresh (..))
import GCL.Range (MaybeRanged (..), Range)
import GHC.Generics (Generic)
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name (Name))

data TypeError
  = NotInScope Name
  | UnifyFailed A.Type A.Type (Maybe Range)
  | RecursiveType Name A.Type (Maybe Range)
  | AssignToConst Name
  | UndefinedType Name
  | DuplicatedIdentifiers [Name]
  | RedundantNames [Name]
  | RedundantExprs [A.Expr]
  | MissingArguments [Name]
  | PatternArityMismatch {- Expected -} Int {- Actual -} Int (Maybe Range)
  deriving (Show, Eq, Generic)

instance MaybeRanged TypeError where
  maybeRangeOf (NotInScope n) = maybeRangeOf n
  maybeRangeOf (UnifyFailed _ _ l) = l
  maybeRangeOf (RecursiveType _ _ l) = l
  maybeRangeOf (AssignToConst n) = maybeRangeOf n
  maybeRangeOf (UndefinedType n) = maybeRangeOf n
  maybeRangeOf (DuplicatedIdentifiers ns) = maybeRangeOf ns
  maybeRangeOf (RedundantNames ns) = maybeRangeOf ns
  maybeRangeOf (RedundantExprs exprs) = maybeRangeOf exprs
  maybeRangeOf (MissingArguments ns) = maybeRangeOf ns
  maybeRangeOf (PatternArityMismatch _ _ l) = l

type TyVar = Name

type TmVar = Name

type Subst = Map TyVar A.Type

type Env = Map TmVar A.Scheme

type PEnv = Map Name [A.Type] -- enviroment for declared procedures

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
  deriving (Ord, Eq)

mkInference :: Inference
mkInference = Inference 0

type TIMonad = RSE (Env, PEnv) Inference

-- XXX: leaky, why is this used more than runToTyped?
runTI :: TIMonad a -> (Env, PEnv) -> Inference -> Result (a, Inference)
runTI = runRSE

evalTI :: TIMonad a -> (Env, PEnv) -> Inference -> Result a
evalTI = evalRSE

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
typeToType = A.mkArrowType

-- XXX: is including `Loc` relevant here?
typeInt :: A.Type
typeInt = A.TBase A.TInt Nothing

typeBool :: A.Type
typeBool = A.TBase A.TBool Nothing

askEnv :: TIMonad Env
askEnv = fst <$> ask

askPEnv :: TIMonad PEnv
askPEnv = snd <$> ask

localEnv :: (Env -> Env) -> TIMonad a -> TIMonad a
localEnv f m = do
  local (first f) m
