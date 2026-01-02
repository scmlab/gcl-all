{-# LANGUAGE OverloadedStrings #-}

module GCL.WP.Explanation where

import Data.Text (Text)
import GCL.Predicate
import GCL.Range (Range)
import Render
import Syntax.Abstract.Operator (tInt)
import qualified Syntax.Abstract.Types as A
import Syntax.Common (Name (..))
import Syntax.Typed

emptyExplain :: Text -> Maybe Range -> Origin
emptyExplain title l =
  Explain
    { originHeader = title,
      originExplanation = mempty,
      originInfMode = Secondary,
      originHighlightPartial = False,
      originRange = l
    }

explainAssignment :: Pred -> Pred -> [Name] -> [Expr] -> Maybe Range -> Origin
explainAssignment pre post vars exprs l =
  Explain
    { originHeader = "Assignment",
      originExplanation =
        "After assignment, the postcondition"
          <> (codeE . render) post
          <> "should be implied by the precondition"
          <> (codeE . render) pre
          <> "after free variables"
          <> sepByCommaE (map (codeE . render) vars)
          <> "have been substituted with"
          <> sepByCommaE (map (codeE . render) exprs),
      originInfMode = Primary,
      originHighlightPartial = False,
      originRange = l
    }

explainAfterLoop :: Pred -> [Expr] -> Maybe Range -> Origin
explainAfterLoop inv guards l =
  Explain
    { originHeader = "InvBase",
      originExplanation =
        "The loop invariant"
          <> (codeE . render) inv
          <> "should remain true while all the guards"
          <> sepByCommaE (map (codeE . render) guards)
          <> "become false after executing the loop",
      originInfMode = Primary,
      originHighlightPartial = True,
      originRange = l
    }

explainTermination :: Pred -> [Expr] -> Expr -> Maybe Range -> Origin
explainTermination inv guards bnd l =
  Explain
    { originHeader = "TermBase",
      originExplanation =
        "When the loop invariant"
          <> (codeE . render) inv
          <> "and one of the guards"
          <> sepByCommaE (map (codeE . render) guards)
          <> "remain true (that is, whilst looping), the bound"
          <> (codeE . render) bnd
          <> "should be greater then"
          <> (codeE . render) (Lit (A.Num 0) tInt Nothing),
      originInfMode = Primary,
      originHighlightPartial = True,
      originRange = l
    }
