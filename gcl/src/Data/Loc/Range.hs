{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Data.Loc.Range
-- Description : Position and range types for source locations
--
-- This module provides the primary API for working with source locations.
--
-- == Usage
--
-- All application code should import this module instead of 'Data.Loc':
--
-- @
-- import Data.Loc.Range (Range, Pos, MaybeRanged(..), Ranged(..), ...)
-- @
--
-- == Design
--
-- * 'Range' represents a span with start and end positions (end-exclusive)
-- * 'Maybe Range' replaces the old 'Loc' type (where 'Nothing' = 'NoLoc')
-- * 'Ranged' typeclass for values that always have a range
-- * 'MaybeRanged' typeclass for values that may or may not have a range
--
-- == Range Representations
--
-- This project uses three different ways to represent ranges:
--
-- 1. 'Data.Loc.Loc' (1-based, end-inclusive)
-- 2. 'Data.Loc.Range.Range' (1-based, end-exclusive)
-- 3. 'Language.LSP.Protocol.Types.Range' (0-based, end-exclusive)
--
-- For example, to represent the first two characters \"AB\" at the beginning of a file:
--
-- 1. 'Loc': (1, 1) ~ (1, 2)
-- 2. 'Range': (1, 1) ~ (1, 3)
-- 3. LSP 'Range': (0, 0) ~ (0, 2)
--
-- == Internal Dependencies
--
-- This module re-exports 'Pos' from 'Data.Loc'.
-- 'Data.Loc' should be considered an internal module and not imported directly
-- by application code.
--
-- == Conversion from Inclusive Locations
--
-- The lexer produces end-inclusive locations ('Data.Loc').
-- Use 'fromInclusiveLoc' to convert to end-exclusive 'Range'.
module Data.Loc.Range
  ( Range (Range), -- only the type and the pattern, the constructor is hidden
    mkRange, -- forcing users to use this constructor
    R (..),
    rangeStart,
    rangeEnd,
    mergeRangesUnsafe,
    mergeRanges,
    rangeSpan,
    within,
    Ranged (..),
    MaybeRanged (..),
    (<--->),
    unRange,
    unR,
    rangeOfR,
    -- Conversion from Data.Loc
    fromInclusiveLoc,
    -- Re-export from Data.Loc for Pos manipulation
    Pos (Pos), -- only the type and the pattern, the constructor is hidden
    mkPos, -- forcing users to use this constructor
    posLine,
    posCol,
    posCoff,
    displayPos,
    extractText,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    object,
    withObject,
    (.:),
    (.=),
  )
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Loc as IncLoc
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty))

--------------------------------------------------------------------------------
-- Pos
--------------------------------------------------------------------------------

-- | Position in a source file.
data Pos = Pos_
  { -- | 1-based line number
    _posLine :: !Int,
    -- | 1-based column number
    _posCol :: !Int
  }
  deriving (Eq, Ord)

-- | Pattern synonym for Pos, use mkPos to construct
pattern Pos :: Int -> Int -> Pos
pattern Pos line col <- Pos_ line col -- "<-" single direction pattern: only for matching, not for constructing

{-# COMPLETE Pos #-}

-- | The only way to construct a Pos
mkPos :: Int -> Int -> Pos
mkPos = Pos_

-- | Get the line number (1-based)
posLine :: Pos -> Int
posLine (Pos_ l _) = l

-- | Get the column number (1-based)
posCol :: Pos -> Int
posCol (Pos_ _ c) = c

-- | Get a synthetic ordering key (not a real byte offset)
-- | Used by IntervalMap for efficient range queries
-- | Formula: line * 10000000 + col (supports up to 10M chars per line)
posCoff :: Pos -> Int
posCoff (Pos_ line col) = line * 10000000 + col

-- | Display position as a string
displayPos :: Pos -> String
displayPos (Pos l c) = show l ++ ":" ++ show c

instance Show Pos where
  show = displayPos

--------------------------------------------------------------------------------
-- Range
--------------------------------------------------------------------------------

data Range = Range_ Pos Pos
  deriving (Eq, Generic)

-- | Pattern synonym for Range, use mkRange to construct
pattern Range :: Pos -> Pos -> Range
pattern Range start end <- Range_ start end -- "<-" single direction pattern: only for matching, not for constructing

{-# COMPLETE Range #-}

-- | The only way to construct a Range
mkRange :: Pos -> Pos -> Range
mkRange start end
  | start <= end = Range_ start end
  | otherwise = error $ "mkRange: start <= end does not hold: start: " ++ show start ++ " end: " ++ show end

-- First by comparing their starting positions and then their ending positions
instance Ord Range where
  compare (Range a b) (Range c d) = case compare a c of
    EQ -> compare b d
    others -> others

instance Show Range where
  show (Range start end) =
    if posLine start == posLine end
      then
        -- Same line: "1:5-7"
        show (posLine start)
          <> ":"
          <> show (posCol start)
          <> "-"
          <> show (posCol end)
      else
        -- Different lines: "1:5-2:10"
        show (posLine start)
          <> ":"
          <> show (posCol start)
          <> "-"
          <> show (posLine end)
          <> ":"
          <> show (posCol end)

-- | Starting position of the range
rangeStart :: Range -> Pos
rangeStart (Range a _) = a

-- | Ending position of the range
rangeEnd :: Range -> Pos
rangeEnd (Range _ b) = b

mergeRangesUnsafe :: [Range] -> Range
mergeRangesUnsafe [] = error "mergeRangesUnsafe: empty list"
mergeRangesUnsafe (x : xs) = foldl (<>) x xs

mergeRanges :: NonEmpty Range -> Range
mergeRanges xs = foldl (<>) (NE.head xs) xs

-- | Calculates the length covered by a range
rangeSpan :: Range -> Int
rangeSpan (Range a b) = posCol b - posCol a

-- | See if a Range is within another Range
within :: Range -> Range -> Bool
within (Range a b) (Range c d) = posCol c <= posCol a && posCol b <= posCol d

-- | Merge two ranges by filling their gap
instance Semigroup Range where
  Range a b <> Range c d = mkRange (min a c) (max b d)

--------------------------------------------------------------------------------

-- | Typeclass for values that always have a range
class Ranged a where
  rangeOf :: a -> Range

instance Ranged Range where
  rangeOf x = x

instance (Ranged a) => Ranged (NonEmpty a) where
  rangeOf xs = rangeOf (NE.head xs) <> rangeOf (NE.last xs)

--------------------------------------------------------------------------------

-- | Typeclass for values that may or may not have a location
class MaybeRanged a where
  maybeRangeOf :: a -> Maybe Range

instance MaybeRanged Range where
  maybeRangeOf = Just

instance MaybeRanged (Maybe Range) where
  maybeRangeOf = id

instance (MaybeRanged a) => MaybeRanged [a] where
  maybeRangeOf = foldr ((<--->) . maybeRangeOf) Nothing

instance (MaybeRanged a) => MaybeRanged (NonEmpty a) where
  maybeRangeOf xs = maybeRangeOf (NE.head xs) <---> maybeRangeOf (NE.last xs)

-- | Merge two Maybe Ranges (like <--> for Loc, but for Maybe Range)
-- Nothing <---> x       = x
-- x       <---> Nothing = x
-- Just a  <---> Just b  = Just (a <> b)
(<--->) :: Maybe Range -> Maybe Range -> Maybe Range
Nothing <---> x = x
x <---> Nothing = x
Just a <---> Just b = Just (a <> b)

infixl 6 <--->

--------------------------------------------------------------------------------

-- | A value of type @R a@ is a value of type @a@ with an associated 'Range', but
-- this location is ignored when performing comparisons.
data R a = R Range a
  deriving (Functor)

unRange :: R a -> a
unRange (R _ a) = a

-- | Alias for unRange (to match unLoc naming convention)
unR :: R a -> a
unR = unRange

instance (Eq x) => Eq (R x) where
  (R _ x) == (R _ y) = x == y

instance (Ord x) => Ord (R x) where
  compare (R _ x) (R _ y) = compare x y

instance (Show x) => Show (R x) where
  show (R _ x) = show x

instance Ranged (R a) where
  rangeOf (R range _) = range

instance MaybeRanged (R a) where
  maybeRangeOf (R range _) = Just range

-- | Get the range of an R value (alias for rangeOf)
rangeOfR :: R a -> Range
rangeOfR (R range _) = range

--------------------------------------------------------------------------------

instance ToJSON Pos where
  toJSON = error "ToJSON Pos is not supported. Use Range for serialization."

instance FromJSON Pos where
  parseJSON = error "FromJSON Pos is not supported."

instance FromJSON Range where
  parseJSON = error "FromJSON Range is not supported."

-- | Convert Range to LSP Range JSON representation
-- TODO: This is actually a "toLSPRangeJSON", not a general ToJSON instance.
instance ToJSON Range where
  toJSON (Range (Pos line col) (Pos line' col')) =
    object
      [ "start"
          .= object
            [ "line" .= (line - 1),
              "character" .= (col - 1)
            ],
        "end"
          .= object
            [ "line" .= (line' - 1),
              "character" .= (col' - 1)
            ]
      ]

--------------------------------------------------------------------------------

instance Pretty Range where
  pretty (Range start end) =
    if posLine start == posLine end
      then
        -- Same line: "1:5-7"
        pretty (posLine start)
          <> ":"
          <> pretty (posCol start)
          <> "-"
          <> pretty (posCol end)
      else
        -- Different lines: "1:5-2:10"
        pretty (posLine start)
          <> ":"
          <> pretty (posCol start)
          <> "-"
          <> pretty (posLine end)
          <> ":"
          <> pretty (posCol end)

instance Pretty Pos where
  pretty = pretty . displayPos

--------------------------------------------------------------------------------
-- Conversion from Data.Loc
--------------------------------------------------------------------------------

-- | Convert an end-inclusive Loc (from Data.Loc) to Maybe Range
-- This converts from end-inclusive to end-exclusive semantics:
-- - end-inclusive: end position is the position of the last character
-- - end-exclusive: end position is one past the last character
-- For example, for the string "AB":
--   - end-inclusive: end column is 2 (pointing to 'B')
--   - end-exclusive: end column is 3 (pointing past 'B')
fromInclusiveLoc :: IncLoc.Loc -> Maybe Range
fromInclusiveLoc IncLoc.NoLoc = Nothing
fromInclusiveLoc (IncLoc.Loc (IncLoc.Pos _ l1 c1 _co1) (IncLoc.Pos _ l2 c2 _co2)) =
  Just $ mkRange (mkPos l1 c1) (mkPos l2 (c2 + 1))

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Extract text covered by a Range from the source text
-- Handles multi-line ranges correctly.
extractText :: Range -> Text -> Text
extractText (Range (Pos l1 c1) (Pos l2 c2)) text = result
  where
    -- l1, l2, c1, c2 are 1-based
    -- l1 / l2 / c1 are inclusive, but c2 is exclusive
    rangeLines = drop (l1 - 1) $ take l2 $ Text.lines text
    resultLines = modifyFirst (Text.drop (c1 - 1)) $ modifyLast (Text.take (c2 - 1)) rangeLines
    result = Text.intercalate "\n" resultLines

    modifyFirst _ [] = []
    modifyFirst f (x : xs) = f x : xs

    modifyLast _ [] = []
    modifyLast f [x] = [f x]
    modifyLast f (x : xs) = x : modifyLast f xs
