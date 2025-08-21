{-# LANGUAGE DeriveGeneric #-}

module Syntax.Parser.Error where

import qualified Data.Aeson.Types as JSON
import Data.List.NonEmpty (NonEmpty)
import Data.Loc
  ( Loc,
    Pos,
  )
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Error
data ParseError
  = LexicalError Pos
  | SyntacticError (NonEmpty (Loc, String)) String -- The second argument is for parsing log.
  deriving (Eq, Show, Generic)
