module Syntax.Common.Instances.Located where

import Data.Loc
import Syntax.Common.Types
import Prelude hiding (Ordering (..))

instance (Located a, Located b) => Located (Either a b) where
  locOf (Left a) = locOf a
  locOf (Right b) = locOf b

instance (Located a) => Located (Name a) where
  locOf (Name _ l) = locOf l

instance (Located a) => Located (ChainOp a) where
  locOf (EQProp l) = locOf l
  locOf (EQPropU l) = locOf l
  locOf (EQ l) = locOf l
  locOf (NEQ l) = locOf l
  locOf (NEQU l) = locOf l
  locOf (LTE l) = locOf l
  locOf (LTEU l) = locOf l
  locOf (GTE l) = locOf l
  locOf (GTEU l) = locOf l
  locOf (LT l) = locOf l
  locOf (GT l) = locOf l

instance (Located a) => Located (ArithOp a) where
  locOf (Implies l) = locOf l
  locOf (ImpliesU l) = locOf l
  locOf (Disj l) = locOf l
  locOf (DisjU l) = locOf l
  locOf (Conj l) = locOf l
  locOf (ConjU l) = locOf l
  locOf (Neg l) = locOf l
  locOf (NegU l) = locOf l
  locOf (NegNum l) = locOf l
  locOf (Add l) = locOf l
  locOf (Sub l) = locOf l
  locOf (Mul l) = locOf l
  locOf (Div l) = locOf l
  locOf (Mod l) = locOf l
  locOf (Max l) = locOf l
  locOf (Min l) = locOf l
  locOf (Exp l) = locOf l
  locOf (Hash l) = locOf l
  locOf (PointsTo l) = locOf l
  locOf (SConj l) = locOf l
  locOf (SImp l) = locOf l

instance (Located a) => Located (TypeOp a) where
  locOf (Arrow l) = locOf l

instance (Located a) => Located (Op a) where
  locOf (ChainOp op) = locOf op
  locOf (ArithOp op) = locOf op
  locOf (TypeOp op) = locOf op
