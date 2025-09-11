{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Common.Instances.Json where

import Data.Aeson
import Data.Loc (Loc (..), Pos (..))
import Syntax.Common.Types

instance (ToJSON a) => ToJSON (Name a) where
  toJSON :: Name a -> Value
  toJSON (Name text loc) =
    object
      [ "symbol" .= String text,
        "location" .= toJSON loc
      ]

instance (ToJSON a) => ToJSONKey (Name a)

instance (ToJSON a) => ToJSON (ChainOp a)

instance (ToJSON a) => ToJSON (ArithOp a)

instance (ToJSON a) => ToJSON (TypeOp a)

instance (ToJSON a) => ToJSON (Op a)

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
