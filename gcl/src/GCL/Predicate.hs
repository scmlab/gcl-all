{-# LANGUAGE DeriveGeneric #-}

module GCL.Predicate where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GCL.Range
  ( MaybeRanged (..),
    Range,
    Ranged (rangeOf),
    within,
  )
import GCL.Type2.Types (Env)
import GHC.Generics (Generic)
import Render.Element
import Syntax.Abstract.Types (Type)
import Syntax.Typed (Expr)

-- | A predicate is an expression, whose type happens to be Bool.
type Pred = Expr

-- | Proof obligation
data PO = PO
  { poPred :: Pred, -- the single proposition `pre ⇒ post`
    poReducedPred :: Pred, -- display-only reduction state
    poStrippedPred :: Text, -- pred without surrounding rendered html
    poProofExists :: Bool,
    poSMTResult :: Text,
    poAnchorRange :: Maybe Range, -- anchor location, if it exists in the source
    poOrigin :: Maybe Range -- whereabouts
  }
  deriving (Eq, Show, Generic)

instance Ord PO where
  compare (PO _ _ _ _ _ _ x) (PO _ _ _ _ _ _ y) = compare y x

instance MaybeRanged PO where
  maybeRangeOf (PO _ _ _ _ _ _ o) = maybeRangeOf o

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
    -- Display-only reductions; refinement always uses the original conditions.
    specReducedPreCond :: Pred,
    specReducedPostCond :: Pred,
    specRange :: Range,
    specTypeEnv :: Env
  }
  deriving (Eq, Show, Generic)

instance MaybeRanged Spec where
  maybeRangeOf Specification {specRange = r} = Just r

instance Ranged Spec where
  rangeOf Specification {specRange = r} = r

data Hole = Hole
  { holeID :: Int,
    holeType :: Type,
    holeRange :: Range,
    holeTypeEnv :: Env,
    holeConstraint :: Expr -> Maybe HoleError
  }
  deriving (Generic)

instance Eq Hole where
  (Hole id1 ty1 r1 env1 _) == (Hole id2 ty2 r2 env2 _) =
    id1 == id2 && ty1 == ty2 && r1 == r2 && env1 == env2

instance Show Hole where
  show (Hole id' ty r env _) = "Hole" <> show (id', ty, r, env)

instance MaybeRanged Hole where
  maybeRangeOf (Hole _ _ r _ _) = Just r

instance Ranged Hole where
  rangeOf (Hole _ _ r _ _) = r

data HoleError = UnsatisfiedConstraint Text (Maybe Range)
  deriving (Eq, Show)

instance MaybeRanged HoleError where
  maybeRangeOf (UnsatisfiedConstraint _ r) = r
