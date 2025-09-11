module Syntax.ConstExpr where

import Data.Char (isLower)
import Data.List (partition)
import Data.Loc (Loc)
import Data.Maybe
  ( listToMaybe,
    mapMaybe,
  )
import qualified Data.Set as Set
import qualified Data.Text as Text
import GCL.Common (freeVars)
import Syntax.Abstract
import Syntax.Common
  ( Name,
    nameToText,
  )

pickGlobals :: [Declaration a] -> ([Expr a], [Expr a])
pickGlobals = partition isGlobalProp . mapMaybe extractAssertion
  where
    -- An assertion is a global prop
    -- if all of its free variables are of CONSTANTS
    isGlobalProp :: Expr a -> Bool
    isGlobalProp assertion = Set.null $ Set.filter nameIsVar (freeVars assertion)

    nameIsVar :: Name a -> Bool
    nameIsVar name =
      maybe False isLower (listToMaybe (Text.unpack (nameToText name)))

    -- Extracts both the assertion and those declared names
    extractAssertion :: Declaration Loc -> Maybe (Expr Loc)
    extractAssertion (ConstDecl _ _ e _) = e
    extractAssertion (VarDecl _ _ e _) = e
