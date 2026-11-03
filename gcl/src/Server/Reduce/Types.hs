{-# LANGUAGE DeriveGeneric #-}

module Server.Reduce.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- PO targets use list indices; specification targets use stable spec IDs.
data ReduceTarget
  = ProofObligation Int
  | SpecificationPrecondition Int
  | SpecificationPostcondition Int
  deriving (Eq, Show, Generic)

instance FromJSON ReduceTarget

instance ToJSON ReduceTarget
