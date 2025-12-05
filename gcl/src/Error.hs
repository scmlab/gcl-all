{-# LANGUAGE DeriveGeneric #-}

module Error where

import qualified Data.Aeson.Types as JSON
import Data.Loc.Range (Range)
import GCL.Type (TypeError)
import GCL.WP.Types (StructError)
import GHC.Generics
import Syntax.Common ()
import Syntax.Parser.Error (ParseError)

--------------------------------------------------------------------------------

-- | Error
data Error
  = ParseError ParseError
  | TypeError TypeError
  | StructError StructError
  | CannotReadFile FilePath
  | Others String String (Maybe Range)
  deriving (Eq, Show)
