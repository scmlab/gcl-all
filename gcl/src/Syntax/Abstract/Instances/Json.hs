module Syntax.Abstract.Instances.Json where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Syntax.Abstract.Types

instance (ToJSON a) => ToJSON (Endpoint a)

instance (ToJSON a) => ToJSON (Interval a)

instance ToJSON TBase

instance (ToJSON a) => ToJSON (Type a)

instance (ToJSON a) => ToJSON (Kind a)

instance (ToJSON a) => ToJSON (Expr a)

instance (ToJSON a) => ToJSON (Chain a)

instance (ToJSON a) => ToJSON (FuncClause a)

instance (ToJSON a) => ToJSON (CaseClause a)

instance (ToJSON a) => ToJSON (Pattern a)

instance ToJSON Lit

instance FromJSON Lit
