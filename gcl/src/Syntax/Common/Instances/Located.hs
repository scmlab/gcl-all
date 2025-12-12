module Syntax.Common.Instances.Located where

import Data.Loc.Range (MaybeRanged (..))
import Syntax.Common.Types
import Prelude hiding (Ordering (..))

instance (MaybeRanged a, MaybeRanged b) => MaybeRanged (Either a b) where
  maybeRangeOf (Left a) = maybeRangeOf a
  maybeRangeOf (Right b) = maybeRangeOf b

instance MaybeRanged Name where
  maybeRangeOf (Name _ l) = l

instance MaybeRanged ChainOp where
  maybeRangeOf (EQProp l) = l
  maybeRangeOf (EQPropU l) = l
  maybeRangeOf (EQ l) = l
  maybeRangeOf (NEQ l) = l
  maybeRangeOf (NEQU l) = l
  maybeRangeOf (LTE l) = l
  maybeRangeOf (LTEU l) = l
  maybeRangeOf (GTE l) = l
  maybeRangeOf (GTEU l) = l
  maybeRangeOf (LT l) = l
  maybeRangeOf (GT l) = l

instance MaybeRanged ArithOp where
  maybeRangeOf (Implies l) = l
  maybeRangeOf (ImpliesU l) = l
  maybeRangeOf (Disj l) = l
  maybeRangeOf (DisjU l) = l
  maybeRangeOf (Conj l) = l
  maybeRangeOf (ConjU l) = l
  maybeRangeOf (Neg l) = l
  maybeRangeOf (NegU l) = l
  maybeRangeOf (NegNum l) = l
  maybeRangeOf (Add l) = l
  maybeRangeOf (Sub l) = l
  maybeRangeOf (Mul l) = l
  maybeRangeOf (Div l) = l
  maybeRangeOf (Mod l) = l
  maybeRangeOf (Max l) = l
  maybeRangeOf (Min l) = l
  maybeRangeOf (Exp l) = l
  maybeRangeOf (Hash l) = l
  maybeRangeOf (PointsTo l) = l
  maybeRangeOf (SConj l) = l
  maybeRangeOf (SImp l) = l

instance MaybeRanged TypeOp where
  maybeRangeOf (Arrow l) = l

instance MaybeRanged Op where
  maybeRangeOf (ChainOp op) = maybeRangeOf op
  maybeRangeOf (ArithOp op) = maybeRangeOf op
  maybeRangeOf (TypeOp op) = maybeRangeOf op
