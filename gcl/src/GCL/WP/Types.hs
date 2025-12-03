{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.WP.Types where

import Control.Monad.Except (Except)
import Control.Monad.RWS
  ( MonadState (..),
    RWST (..),
  )
import Data.Aeson (ToJSON)
import Data.IntMap (IntMap)
import Data.Loc.Range (Range, MaybeRanged (..), maybeRangeToLoc)
import Data.Loc (Located (..))
import Data.Map (Map)
import Data.Text (Text)
import GCL.Common
import GCL.Predicate
  ( InfMode (..),
    PO (..),
    Pred,
    Spec (..),
  )
import GHC.Generics (Generic)
import Syntax.Typed

-- The WP monad.

type TM = Except StructError

type Decls = Map Text (Maybe Expr)

type WP =
  RWST
    (Decls, [[Text]])
    ([PO], [Spec], [StructWarning], IntMap (Int, Expr))
    Int
    TM

instance Counterous WP where
  countUp = do
    i <- get
    put (succ i)
    return i

---

-- will be constructed by groupStmts
data SegElm
  = SAsrt Stmt
  | SSpec Stmt
  | SStmts [Stmt]

-- types of mutually recursive functions

type TstructStmts = InfMode -> (Pred, Maybe Expr) -> [Stmt] -> Pred -> WP ()

type TstructSegs = (Pred, Maybe Expr) -> [SegElm] -> Pred -> WP ()

type Tstruct = (Pred, Maybe Expr) -> Stmt -> Pred -> WP ()

type TwpSegs = [SegElm] -> Pred -> WP Pred

type TwpSStmts = [Stmt] -> Pred -> WP Pred

type Twp = Stmt -> Pred -> WP Pred

type TspStmts = (Pred, Maybe Expr) -> [Stmt] -> WP Pred

type TspSegs = (Pred, Maybe Expr) -> [SegElm] -> WP Pred

type TspSStmts = (Pred, Maybe Expr) -> [Stmt] -> WP Pred

---

data StructWarning
  = MissingBound Range
  deriving (Eq, Show, Generic)

instance MaybeRanged StructWarning where
  maybeRangeOf (MissingBound rng) = Just rng

instance Located StructWarning where
  locOf = maybeRangeToLoc . maybeRangeOf

data StructError
  = MissingAssertion (Maybe Range)
  | MissingPostcondition (Maybe Range)
  | MultiDimArrayAsgnNotImp (Maybe Range)
  | -- Assignment to multi-dimensional array not implemented.
    -- SCM: will remove this when we figure out how.
    LocalVarExceedScope (Maybe Range)
  deriving (Eq, Show, Generic)

instance MaybeRanged StructError where
  maybeRangeOf (MissingAssertion l) = l
  maybeRangeOf (MissingPostcondition l) = l
  maybeRangeOf (MultiDimArrayAsgnNotImp l) = l
  maybeRangeOf (LocalVarExceedScope l) = l

instance Located StructError where
  locOf = maybeRangeToLoc . maybeRangeOf

-- freshPreInScope prefix scope
--   generates a fresh name, with prefix, that does not appear in scope
