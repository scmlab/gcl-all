{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Common.Instances.Json where

import Data.Aeson
import Data.Loc.Range ()
-- Import ToJSON Range instance
import Syntax.Common.Types

instance ToJSON Name where
  toJSON :: Name -> Value
  toJSON (Name text loc) =
    object
      [ "symbol" .= String text,
        "location" .= toJSON loc
      ]

instance ToJSONKey Name

instance ToJSON ChainOp

instance ToJSON ArithOp

instance ToJSON TypeOp

instance ToJSON Op
