{-# LANGUAGE OverloadedStrings #-}

module Pretty.Predicate where

import GCL.Spec
import GCL.WP.Types
import Pretty.Abstract ()
import Pretty.Util
import Prettyprinter
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | Obligation & Specification
instance Pretty PO where
  pretty = fromRenderSection

instance Pretty Spec where
  pretty = fromRenderSection
