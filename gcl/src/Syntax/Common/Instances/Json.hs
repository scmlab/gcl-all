{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax.Common.Instances.Json where

import Data.Aeson
import Data.Loc (Loc (..), Pos (..))
import Syntax.Common.Types

instance ToJSON Name where
  toJSON :: Name -> Value
  toJSON (Name text loc) =
    object
      [ "symbol" .= String text,
        "location" .= toJSON loc
      ]

instance ToJSONKey Name

instance ToJSON (ChainOp Loc)

instance ToJSON (ArithOp Loc)

instance ToJSON (TypeOp Loc)

instance ToJSON (Op Loc)

instance ToJSON Loc where
  toJSON :: Loc -> Value
  toJSON NoLoc = Null
  toJSON (Loc (Pos filePath line column _) (Pos _ line' column' _)) =
    object
      [ "filePath" .= toJSON filePath,
        "start"
          .= object
            [ "line" .= (line - 1),
              "character" .= (column - 1)
            ],
        "end"
          .= object
            [ "line" .= (line' - 1),
              "character" .= (column' - 1)
            ]
      ]
