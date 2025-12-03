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
-- == Internal Dependencies
--
-- This module re-exports types from 'Data.Loc' (e.g., 'Pos', 'L', 'Located')
-- for backward compatibility with the lexer\/parser layer.
-- 'Data.Loc' should be considered an internal module and not imported directly
-- by application code.
--
-- == Conversion from Inclusive Locations
--
-- The lexer produces end-inclusive locations ('Data.Loc.Inclusive').
-- Use 'fromInclusiveLoc' to convert to end-exclusive 'Range'.

module Data.Loc.Range
  ( Range (Range), -- only the type and the pattern, the constructor is hidden
    mkRange, -- forcing users to use this constructor
    R (..),
    ShortRange (..),
    rangeStart,
    rangeEnd,
    rangeFile,
    fromLoc,
    toLoc,
    toMaybeRange,
    maybeRangeToLoc,
    fromLocs,
    mergeRangesUnsafe,
    mergeRanges,
    rangeSpan,
    within,
    Ranged (..),
    MaybeRanged (..),
    (<->>),
    unRange,
    rangeOfR,
    compareWithPosition,
    withinRange,
    compareWithPositionR,
    withinRangeR,
    -- Conversion from Data.Loc.Inclusive
    fromInclusiveLoc,
    fromInclusivePos,
    -- Re-export from Data.Loc for convenience (Lexer/Parser compatibility)
    Pos (..),
    posLine,
    posCol,
    posFile,
    posCoff,
    displayPos,
    startPos,
    advancePos,
    -- Re-export L and unLoc for Lexer/Parser compatibility
    L (..),
    unLoc,
    Loc (..),
    Located (..),
    (<-->),
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
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Loc hiding (fromLoc)
import qualified Data.Loc.Inclusive as Inc
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty))

-- TODO: write a short documentation about Range and its usage
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
        posFile start
          <> " ["
          <> show (posCoff start)
          <> "-"
          <> show (posCoff end)
          <> "] "
          <> show (posLine start)
          <> ":"
          <> show (posCol start)
          <> "-"
          <> show (posCol end)
      else
        posFile start
          <> " ["
          <> show (posCoff start)
          <> "-"
          <> show (posCoff end)
          <> "] "
          <> show (posLine start)
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

-- | Filepath of the range
rangeFile :: Range -> FilePath
rangeFile (Range a _) = posFile a

-- | Loc -> Maybe Range
fromLoc :: Loc -> Maybe Range
fromLoc NoLoc = Nothing
fromLoc (Loc x y) = Just (mkRange x y)

-- | Loc -> Maybe Range (alias for fromLoc with clearer naming)
toMaybeRange :: Loc -> Maybe Range
toMaybeRange = fromLoc

-- | Maybe Range -> Loc (convert back to Loc for backward compatibility)
maybeRangeToLoc :: Maybe Range -> Loc
maybeRangeToLoc Nothing = NoLoc
maybeRangeToLoc (Just r) = toLoc r

-- | Range -> Loc
toLoc :: Range -> Loc
toLoc (Range x y) = Loc x y

-- | [Loc] -> [Range]
fromLocs :: [Loc] -> [Range]
fromLocs = mapMaybe fromLoc

mergeRangesUnsafe :: [Range] -> Range
mergeRangesUnsafe xs = foldl (<>) (head xs) xs

mergeRanges :: NonEmpty Range -> Range
mergeRanges xs = foldl (<>) (NE.head xs) xs

-- | Calculates the length covered by a range
rangeSpan :: Range -> Int
rangeSpan (Range a b) = posCol b - posCol a

-- | See if a Range is within another Range
within :: Range -> Range -> Bool
within (Range a b) (Range c d) = posCol c <= posCol a && posCol b <= posCol d

instance Located Range where
  locOf (Range x y) = Loc x y

-- | Merge two ranges by filling their gap
instance Semigroup Range where
  Range a b <> Range c d = case (a `compare` c, b `compare` d) of
    (LT, LT) -> mkRange a d
    (LT, EQ) -> mkRange a d
    (LT, GT) -> mkRange a b
    (EQ, LT) -> mkRange a d
    (EQ, EQ) -> mkRange a b
    (EQ, GT) -> mkRange a b
    (GT, LT) -> mkRange c d
    (GT, EQ) -> mkRange c d
    (GT, GT) -> mkRange c b

--------------------------------------------------------------------------------

-- | Like "Located"
class Ranged a where
  rangeOf :: a -> Range

instance Ranged Range where
  rangeOf x = x

instance (Ranged a) => Ranged (NonEmpty a) where
  rangeOf xs = rangeOf (NE.head xs) <> rangeOf (NE.last xs)

--------------------------------------------------------------------------------

-- | Like "Located" but for types that may not have a location (replaces NoLoc with Nothing)
class MaybeRanged a where
  maybeRangeOf :: a -> Maybe Range

instance MaybeRanged Range where
  maybeRangeOf = Just

instance MaybeRanged (Maybe Range) where
  maybeRangeOf = id

instance (MaybeRanged a) => MaybeRanged [a] where
  maybeRangeOf = foldr ((<->>) . maybeRangeOf) Nothing

instance (MaybeRanged a) => MaybeRanged (NonEmpty a) where
  maybeRangeOf xs = maybeRangeOf (NE.head xs) <->> maybeRangeOf (NE.last xs)

-- | Merge two Maybe Ranges (like <--> for Loc, but for Maybe Range)
-- Nothing <->> x       = x
-- x       <->> Nothing = x
-- Just a  <->> Just b  = Just (a <> b)
(<->>) :: Maybe Range -> Maybe Range -> Maybe Range
Nothing <->> x = x
x <->> Nothing = x
Just a <->> Just b = Just (a <> b)

infixl 6 <->>

--------------------------------------------------------------------------------

-- | A value of type @R a@ is a value of type @a@ with an associated 'Range', but
-- this location is ignored when performing comparisons.
data R a = R Range a
  deriving (Functor)

unRange :: R a -> a
unRange (R _ a) = a

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

-- | Make Pos instances of FromJSON and ToJSON
instance ToJSON Pos where
  toJSON (Pos file line col byte) =
    object
      [ "file" .= file,
        "line" .= line,
        "column" .= col,
        "byte" .= byte
      ]

instance FromJSON Pos where
  parseJSON = withObject "Pos" $ \v ->
    Pos
      <$> v .: "file"
      <*> v .: "line"
      <*> v .: "column"
      <*> v .: "byte"

-- | Make Range instances  of FromJSON and ToJSON
instance FromJSON Range where
  parseJSON = withObject "Range" $ \v ->
    mkRange
      <$> v .: "start"
      <*> v .: "end"

instance ToJSON Range where
  toJSON (Range start end) =
    object
      [ "start" .= start,
        "end" .= end
      ]

--------------------------------------------------------------------------------

-- | Compare the cursor position with something
--  EQ: the cursor is placed within that thing
--  LT: the cursor is placed BEFORE (but not touching) that thing
--  GT: the cursor is placed AFTER (but not touching) that thing
compareWithPosition :: (Located a) => Pos -> a -> Ordering
compareWithPosition pos x = case locOf x of
  NoLoc -> EQ
  Loc start end ->
    if posCoff pos < posCoff start
      then LT
      else if posCoff pos > posCoff end then GT else EQ

-- | See if something is within the selection
withinRange :: (Located a) => Range -> a -> Bool
withinRange (Range left right) x =
  compareWithPosition left x
    == EQ
    || compareWithPosition right x
      == EQ
    || (compareWithPosition left x == LT && compareWithPosition right x == GT)

-- | Compare the cursor position with something (MaybeRanged version)
--  EQ: the cursor is placed within that thing
--  LT: the cursor is placed BEFORE (but not touching) that thing
--  GT: the cursor is placed AFTER (but not touching) that thing
compareWithPositionR :: (MaybeRanged a) => Pos -> a -> Ordering
compareWithPositionR pos x = case maybeRangeOf x of
  Nothing -> EQ
  Just (Range start end) ->
    if posCoff pos < posCoff start
      then LT
      else if posCoff pos > posCoff end then GT else EQ

-- | See if something is within the selection (MaybeRanged version)
withinRangeR :: (MaybeRanged a) => Range -> a -> Bool
withinRangeR (Range left right) x =
  compareWithPositionR left x
    == EQ
    || compareWithPositionR right x
      == EQ
    || (compareWithPositionR left x == LT && compareWithPositionR right x == GT)

--------------------------------------------------------------------------------

instance Pretty Range where
  pretty (Range start end) =
    if posLine start == posLine end
      then
        pretty (posFile start)
          <> " ["
          <> pretty (posCoff start)
          <> "-"
          <> pretty (posCoff end)
          <> "] "
          <> pretty (posLine start)
          <> ":"
          <> pretty (posCol start)
          <> "-"
          <> pretty (posCol end)
      else
        pretty (posFile start)
          <> " ["
          <> pretty (posCoff start)
          <> "-"
          <> pretty (posCoff end)
          <> "] "
          <> pretty (posLine start)
          <> ":"
          <> pretty (posCol start)
          <> "-"
          <> pretty (posLine end)
          <> ":"
          <> pretty (posCol end)

instance Pretty Loc where
  pretty = pretty . displayLoc

instance Pretty Pos where
  pretty = pretty . displayPos

--------------------------------------------------------------------------------

-- | Like Range but a special  Show &Pretty instance, won't display the full path
newtype ShortRange = ShortRange {unShortRange :: Range}

instance Show ShortRange where
  show (ShortRange (Range start end)) =
    let path = case split '/' (posFile start) of
          [] -> []
          xs -> last xs
     in if posLine start == posLine end
          then
            path
              <> " ["
              <> show (posCoff start)
              <> "-"
              <> show (posCoff end)
              <> "] "
              <> show (posLine start)
              <> ":"
              <> show (posCol start)
              <> "-"
              <> show (posCol end)
          else
            path
              <> " ["
              <> show (posCoff start)
              <> "-"
              <> show (posCoff end)
              <> "] "
              <> show (posLine start)
              <> ":"
              <> show (posCol start)
              <> "-"
              <> show (posLine end)
              <> ":"
              <> show (posCol end)
    where
      split :: Char -> String -> [String]
      split c = filter (/= [c]) . List.groupBy (\x y -> x /= c && y /= c)

instance Pretty ShortRange where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Conversion from Data.Loc.Inclusive
--------------------------------------------------------------------------------

-- | Convert an end-inclusive Pos (from Data.Loc.Inclusive) to our Pos
-- Note: The Pos type is structurally identical, but we explicitly convert
-- to make the semantics clear.
fromInclusivePos :: Inc.Pos -> Pos
fromInclusivePos (Inc.Pos f l c co) = Pos f l c co

-- | Convert an end-inclusive Loc (from Data.Loc.Inclusive) to Maybe Range
-- This converts from end-inclusive to end-exclusive semantics:
-- - end-inclusive: end position is the position of the last character
-- - end-exclusive: end position is one past the last character
-- For example, for the string "AB":
--   - end-inclusive: end column is 2 (pointing to 'B')
--   - end-exclusive: end column is 3 (pointing past 'B')
fromInclusiveLoc :: Inc.Loc -> Maybe Range
fromInclusiveLoc Inc.NoLoc = Nothing
fromInclusiveLoc (Inc.Loc (Inc.Pos f1 l1 c1 co1) (Inc.Pos f2 l2 c2 co2)) =
  Just $ mkRange (Pos f1 l1 c1 co1) (Pos f2 l2 (c2 + 1) (co2 + 1))
