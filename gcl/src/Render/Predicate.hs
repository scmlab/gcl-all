{-# LANGUAGE OverloadedStrings #-}

module Render.Predicate where

import qualified Data.Text as Text
import GCL.Range (MaybeRanged (..))
import GCL.Spec
import GCL.WP.Types
import Render.Class
import Render.Element
import Render.Syntax.Abstract ()
import Render.Syntax.Typed (renderPOPredRZ)

instance Render StructWarning where
  render (MissingBound _) =
    "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""

instance RenderSection StructWarning where
  renderSection x = case x of
    MissingBound range ->
      Section Yellow [Header "Bound Missing" (Just range), Paragraph (render x)]

instance RenderSection Spec where
  renderSection Specification {specPreCond = pre, specPostCond = post, specRange = range} =
    Section
      Blue
      [ Header "Precondition" (Just range),
        Code (render pre),
        Header "Postcondition" Nothing,
        Code (render post)
      ]

-- This instance is used only for static Pretty output, which discards redex
-- annotations. Interactive PO rendering goes through Server.ToClient with
-- fsDefinitions, so no definition environment is needed here.
instance RenderSection PO where
  renderSection (PO _predicate reducedPredicate _ _ _ _ _ origin) =
    Section Plain $
      [ Header
          "" -- (Text.pack $ show $ render origin)
          origin
      ]
        <> [Code (renderPOPredRZ [] reducedPredicate)]
