{-# LANGUAGE DeriveGeneric #-}

module Syntax.Parser.Error where

import qualified Data.Aeson.Types as JSON
import Data.List.NonEmpty (NonEmpty)
import Data.Loc.Range (Pos, Range)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Error
data ParseError
  = LexicalError Pos
  | SyntacticError (NonEmpty (Maybe Range, String)) String -- The second argument is for parsing log.
  deriving (Eq, Show, Generic)
