module Syntax.Abstract.Instances.Json where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Syntax.Abstract.Types

instance ToJSON Endpoint

instance ToJSON Interval

instance ToJSON TBase

instance ToJSON Type

instance ToJSON Kind

instance ToJSON Expr

instance ToJSON Chain

instance ToJSON FuncClause

instance ToJSON CaseClause

instance ToJSON Pattern

instance ToJSON Lit

instance FromJSON Lit
