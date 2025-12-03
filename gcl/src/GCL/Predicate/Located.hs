module GCL.Predicate.Located where

-- This module is deprecated. Use MaybeRanged from Data.Loc.Range instead.
-- The Located instances here have been commented out as part of the migration
-- from Loc to Maybe Range.

{-
import Data.Loc (Loc (..), Located, locOf)
import GCL.Predicate (Pred (..))

instance Located Pred where
  locOf (Constant _) = NoLoc
  locOf (GuardIf _ l) = l
  locOf (GuardLoop _ l) = l
  locOf (Assertion _ l) = l
  locOf (LoopInvariant _ _ l) = l
  locOf (Bound _ l) = l
  locOf (Conjunct _) = NoLoc
  locOf (Disjunct _) = NoLoc
  locOf (Negate _) = NoLoc

instance Located Stmt where
  locOf (Skip l) = locOf l
  locOf (Abort l) = locOf l
  locOf (Assign l _ _) = locOf l
  locOf (Do l _ _) = locOf l
  locOf (If l _) = locOf l
  locOf (Spec l _) = locOf l
-}
