{-# LANGUAGE DeriveGeneric #-}

module GCL.Spec where

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

data Spec = Specification
  { specID :: Int,
    specPreCond :: Expr,
    specPostCond :: Expr,
    -- Display-only reductions; refinement always uses the original conditions.
    specReducedPreCond :: Expr,
    specReducedPostCond :: Expr,
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
