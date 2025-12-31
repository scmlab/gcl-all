{-# LANGUAGE DeriveGeneric #-}

module GCL.Predicate where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON
import GCL.Range
  ( MaybeRanged (..),
    Range,
    Ranged (rangeOf),
    within,
  )
import qualified Data.Set as Set
import Data.Text (Text)
import GCL.Common
import GHC.Generics (Generic)
import Render.Element
import Syntax.Common (Name)
import Syntax.Typed (Expr)

-- | A predicate is an expression, whose type happens to be Bool.
type Pred = Expr

{-
-- | Predicates
data Pred
  = Constant Expr
  | GuardIf Expr Loc
  | GuardLoop Expr Loc
  | Assertion Expr Loc
  | LoopInvariant Expr Expr Loc -- predicate & bound
  | Bound Expr Loc
  | Conjunct [Pred]
  | Disjunct [Pred]
  | Negate Pred
  deriving (Eq, Show, Generic)

instance ToJSON Pred

instance Free Pred where
  freeVars (Constant e) = freeVars e
  freeVars (GuardIf e _) = freeVars e
  freeVars (GuardLoop e _) = freeVars e
  freeVars (Assertion e _) = freeVars e
  freeVars (LoopInvariant e1 e2 _) = freeVars e1 <> freeVars e2 -- predicate & bound
  freeVars (Bound e _) = freeVars e
  freeVars (Conjunct es) = Set.unions $ map freeVars es
  freeVars (Disjunct es) = Set.unions $ map freeVars es
  freeVars (Negate e) = freeVars e

--------------------------------------------------------------------------------

-- | Data structure for storing Assertions in a program
--
--    { P }   ----- Struct { P } ----
--    stmt
--    stmt
--    stmt
--    { Q }    ----- Struct { Q } ----
--    stmt
--    stmt
--    stmt
--    { R }    ----- Postcond { R } ----
data Struct
  = Struct
      Pred --  assertion
      [Stmt] --  statements after the assertion
      Struct --  the next chunk of assertion with statements after it
  | Postcond Pred
  deriving (Eq)

--------------------------------------------------------------------------------

-- | Statement with its computed precondition
data Stmt
  = Skip (L Pred)
  | Abort (L Pred)
  | Assign (L Pred) [Name] [Expr]
  | Do (L Pred) Expr [GdCmd]
  | If (L Pred) [GdCmd]
  | Spec (L Pred) Pred -- pre and post

data GdCmd = GdCmd Pred Struct
  deriving Eq

instance Eq Stmt where
  Skip  l       == Skip  m       = l == m
  Abort l       == Abort m       = l == m
  Assign l _ _  == Assign m _ _  = l == m
  Do     l _ xs == Do     m _ ys = l == m && xs == ys
  If   l xs     == If   m ys     = l == m && xs == ys
  Spec l _      == Spec m _      = l == m
  _             == _             = False
-}
--------------------------------------------------------------------------------

-- | Proof obligation
data PO = PO
  { poPre :: Pred, -- precondition
    poPost :: Pred, -- post-condition
    poAnchorHash :: Text, -- anchor hash
    poAnchorRange :: Maybe Range, -- anchor location, if it exists in the source
    poOrigin :: Origin -- whereabouts
  }
  deriving (Eq, Show, Generic)

instance Ord PO where
  compare (PO _ _ _ _ x) (PO _ _ _ _ y) = compare y x

instance MaybeRanged PO where
  maybeRangeOf (PO _ _ _ _ o) = maybeRangeOf o

-- instance ToJSON PO

data InfMode
  = Primary -- the main inference mode
  | Secondary -- non-functional postconditions. ignore assertions
  deriving (Eq, Show, Generic)

instance ToJSON InfMode

data Origin
  = AtAbort (Maybe Range)
  | AtSkip (Maybe Range)
  | AtSpec (Maybe Range)
  | AtAssignment (Maybe Range)
  | AtAssertion (Maybe Range) -- AssertSufficient
  | AtIf (Maybe Range)
  | AtLoop (Maybe Range)
  | AtTermination (Maybe Range)
  | Explain
      { originHeader :: Text, -- the text you see on the top of a PO
        originExplanation :: Inlines, -- the text you see at the bottom of a PO (after clicking the header)
        originInfMode :: InfMode,
        originHighlightPartial :: Bool, -- for highlighting only "if" in conditionals and "do" in loops
        originRange :: Maybe Range
      }
  deriving (Eq, Show, Generic)

-- | This ordering would affect how they are presented to the user
-- | A PO should be placed in front of another PO when:
-- |  1. its range is within another PO
-- |  2. its range is ahead of that of another PO
instance Ord Origin where
  compare x y = case maybeRangeOf x of
    Nothing -> LT
    Just a -> case maybeRangeOf y of
      Nothing -> GT
      Just b ->
        if a `within` b then LT else if b `within` a then GT else compare a b

instance MaybeRanged Origin where
  maybeRangeOf (AtAbort l) = l
  maybeRangeOf (AtSkip l) = l
  maybeRangeOf (AtSpec l) = l
  maybeRangeOf (AtAssignment l) = l
  maybeRangeOf (AtAssertion l) = l
  maybeRangeOf (AtIf l) = l
  maybeRangeOf (AtLoop l) = l
  maybeRangeOf (AtTermination l) = l
  maybeRangeOf (Explain _ _ _ _ l) = l

data Spec = Specification
  { specID :: Int,
    specPreCond :: Pred,
    specPostCond :: Pred,
    specRange :: Range,
    specTypeEnv :: [(Index, TypeInfo)]
  }
  deriving (Eq, Show, Generic)

instance MaybeRanged Spec where
  maybeRangeOf (Specification _ _ _ r _) = Just r

instance Ranged Spec where
  rangeOf (Specification _ _ _ r _) = r
