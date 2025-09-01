{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Server.PositionMapping
  ( PositionMapping (..),
    PositionResult (..),
    lowerRange,
    upperRange,
    positionResultToMaybe,
    fromCurrentPosition,
    toCurrentPosition,
    PositionDelta (..),
    -- addOldDelta,
    idDelta,
    -- composeDelta,
    mkDelta,
    toCurrentRange,
    fromCurrentRange,
    applyChange,
    zeroMapping,
    toCurrentRange',
    fromCurrentRange',
  )
where

import Control.DeepSeq
import Control.Monad
import Data.List (foldl')
import qualified Data.Text as T
import qualified Hack
import Language.LSP.Protocol.Types
  ( Position (Position),
    Range (Range),
    TextDocumentContentChangeEvent (TextDocumentContentChangeEvent),
    TextDocumentContentChangePartial (TextDocumentContentChangePartial),
    type (|?) (..),
  )

-- | Either an exact position, or the range of text that was substituted
data PositionResult a
  = -- | Fields need to be non-strict otherwise bind is exponential
    PositionRange
      { unsafeLowerRange :: a,
        unsafeUpperRange :: a
      }
  | PositionExact !a
  deriving (Eq, Ord, Show, Functor)

lowerRange :: PositionResult a -> a
lowerRange (PositionExact a) = a
lowerRange (PositionRange lower _) = lower

upperRange :: PositionResult a -> a
upperRange (PositionExact a) = a
upperRange (PositionRange _ upper) = upper

positionResultToMaybe :: PositionResult a -> Maybe a
positionResultToMaybe (PositionExact a) = Just a
positionResultToMaybe _ = Nothing

instance Applicative PositionResult where
  pure = PositionExact
  (PositionExact f) <*> a = fmap f a
  (PositionRange f g) <*> (PositionExact a) = PositionRange (f a) (g a)
  (PositionRange f g) <*> (PositionRange lower upper) = PositionRange (f lower) (g upper)

instance Monad PositionResult where
  (PositionExact a) >>= f = f a
  (PositionRange lower upper) >>= f = PositionRange lower' upper'
    where
      lower' = lowerRange $ f lower
      upper' = upperRange $ f upper

-- The position delta is the difference between two versions
data PositionDelta = PositionDelta
  { toDelta :: !(Position -> PositionResult Position),
    fromDelta :: !(Position -> PositionResult Position)
  }

instance Show PositionDelta where
  show PositionDelta {} = "PositionDelta{..}"

instance NFData PositionDelta where
  rnf (PositionDelta a b) = a `seq` b `seq` ()

-- A position mapping is the difference from the current version to
-- a specific version
newtype PositionMapping = PositionMapping PositionDelta

fromCurrentPosition :: PositionMapping -> Position -> Maybe Position
fromCurrentPosition (PositionMapping pd) = positionResultToMaybe . fromDelta pd

toCurrentPosition :: PositionMapping -> Position -> Maybe Position
toCurrentPosition (PositionMapping pd) = positionResultToMaybe . toDelta pd

toCurrentRange :: PositionMapping -> Range -> Maybe Range
toCurrentRange mapping (Range a b) =
  Range <$> toCurrentPosition mapping a <*> toCurrentPosition mapping b

toCurrentRange' :: PositionDelta -> Range -> Maybe Range
toCurrentRange' = toCurrentRange . PositionMapping

fromCurrentRange :: PositionMapping -> Range -> Maybe Range
fromCurrentRange mapping (Range a b) =
  Range <$> fromCurrentPosition mapping a <*> fromCurrentPosition mapping b

fromCurrentRange' :: PositionDelta -> Range -> Maybe Range
fromCurrentRange' = fromCurrentRange . PositionMapping

zeroMapping :: PositionMapping
zeroMapping = PositionMapping idDelta

{-
-- | Compose two position mappings. Composes in the same way as function
-- composition (ie the second argument is applied to the position first).
composeDelta ::
  PositionDelta ->
  PositionDelta ->
  PositionDelta
composeDelta (PositionDelta to1 from1) (PositionDelta to2 from2) =
  PositionDelta
    (to1 <=< to2)
    (from1 >=> from2)
-}

idDelta :: PositionDelta
idDelta = PositionDelta pure pure

-- | Convert a set of changes into a delta from k  to k + 1
mkDelta :: [TextDocumentContentChangeEvent] -> PositionDelta
mkDelta cs = foldl' applyChange idDelta cs

{-
-- | addOldDelta
-- Add a old delta onto a Mapping k n to make a Mapping (k - 1) n
addOldDelta ::
  -- | delta from version k - 1 to version k
  PositionDelta ->
  -- | The input mapping is from version k to version n
  PositionMapping ->
  -- | The output mapping is from version k - 1 to version n
  PositionMapping
addOldDelta delta (PositionMapping pm) = PositionMapping (composeDelta pm delta)
-}

-- TODO: We currently ignore the right hand side (if there is only text), as
-- that was what was done with lsp* 1.6 packages
applyChange :: PositionDelta -> TextDocumentContentChangeEvent -> PositionDelta
applyChange PositionDelta {toDelta, fromDelta} (TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial range _ text))) =
  let ci = mkChangeInfo range text
      toCurrent = mkToCurrent ci
      fromCurrent = mkFromCurrent ci
   in PositionDelta
        { toDelta = toCurrent <=< toDelta,
          fromDelta = fromDelta <=< fromCurrent
        }
applyChange posMapping _ = posMapping

posToInt :: Position -> (Int, Int)
posToInt (Position l c) = (fromIntegral l, fromIntegral c)

intToPos :: Int -> Int -> Position
intToPos !l !c = Position (Hack.intToUInt l) (Hack.intToUInt c)

data ChangeInfo = ChangeInfo
  { start :: !Position,
    end :: !Position,
    startLine :: !Int,
    startColumn :: !Int,
    endLine :: !Int,
    endColumn :: !Int,
    linesDiff :: !Int,
    newEndLine :: !Int,
    newEndColumn :: !Int
  }
  deriving (Eq, Show)

mkChangeInfo :: Range -> T.Text -> ChangeInfo
mkChangeInfo (Range start end) txt =
  ChangeInfo {start, end, startLine, startColumn, endLine, endColumn, linesDiff, newEndLine, newEndColumn}
  where
    (startLine, startColumn) = posToInt start
    (endLine, endColumn) = posToInt end
    !linesOld = endLine - startLine -- intermediate
    !linesNew = T.count "\n" txt -- intermediate
    !linesDiff = linesNew - linesOld
    !newEndLine = endLine + linesDiff
    !newEndColumn
      | linesNew == 0 = startColumn + T.length txt
      | otherwise = T.length $ T.takeWhileEnd (/= '\n') txt

mkToCurrent :: ChangeInfo -> (Position -> PositionResult Position)
mkToCurrent ChangeInfo {start, end, startLine, startColumn, endLine, endColumn, linesDiff, newEndLine, newEndColumn} = toCurrent
  where
    toCurrent pos@(posToInt -> (line, column))
      -- Position is before the change and thereby unchanged.
      | line < startLine || line == startLine && column < startColumn =
          PositionExact pos
      -- Position is after the change so increase line and column number as necessary.
      | line > endLine || line == endLine && column >= endColumn =
          let !newLine = line + linesDiff
              !newColumn
                | line == endLine = column + (newEndColumn - endColumn)
                | otherwise = column
           in PositionExact $ intToPos newLine newColumn
      -- Position is in the region that was changed.
      | otherwise = PositionRange start end

mkFromCurrent :: ChangeInfo -> (Position -> PositionResult Position)
mkFromCurrent ChangeInfo {start, end, startLine, startColumn, endLine, endColumn, linesDiff, newEndLine, newEndColumn} = fromCurrent
  where
    fromCurrent pos@(posToInt -> (line, column))
      -- Position is before the change and thereby unchanged
      | line < startLine || line == startLine && column < startColumn =
          PositionExact pos
      -- Position is after the change so increase line and column number as necessary.
      | line > newEndLine || line == newEndLine && column >= newEndColumn =
          let !newLine = line - linesDiff
              !newColumn
                | line == newEndLine = column + (endColumn - newEndColumn)
                | otherwise = column
           in PositionExact $ intToPos newLine newColumn
      -- Position is in the region that was changed.
      | otherwise = PositionRange start end

{-
deltaFromDiff :: T.Text -> T.Text -> PositionDelta
deltaFromDiff (T.lines -> old) (T.lines -> new) =
  PositionDelta (lookupPos (fromIntegral lnew) o2nPrevs o2nNexts old2new) (lookupPos (fromIntegral lold) n2oPrevs n2oNexts new2old)
  where
    !lnew = length new
    !lold = length old

    diff = getDiff old new

    (V.fromList -> !old2new, V.fromList -> !new2old) = go diff 0 0

    -- Compute previous and next lines that mapped successfully
    !o2nPrevs = V.prescanl' f (-1) old2new
    !o2nNexts = V.prescanr' (flip f) lnew old2new

    !n2oPrevs = V.prescanl' f (-1) new2old
    !n2oNexts = V.prescanr' (flip f) lold new2old

    f :: Int -> Int -> Int
    f !a !b = if b == -1 then a else b

    lookupPos :: Int -> V.Vector Int -> V.Vector Int -> V.Vector Int -> Position -> PositionResult Position
    lookupPos end prevs nexts xs (Position line col)
      | line >= fromIntegral (V.length xs) = PositionRange (Position (Hack.intToUInt end) 0) (Position (Hack.intToUInt end) 0)
      | otherwise = case V.unsafeIndex xs (fromIntegral line) of
          -1 ->
            -- look for the previous and next lines that mapped successfully
            let !prev = 1 + V.unsafeIndex prevs (fromIntegral line)
                !next = V.unsafeIndex nexts (fromIntegral line)
             in PositionRange (Position (fromIntegral prev) 0) (Position (fromIntegral next) 0)
          line' -> PositionExact (Position (fromIntegral line') col)

    -- Construct a mapping between lines in the diff
    -- -1 for unsuccessful mapping
    go :: [Diff T.Text] -> Int -> Int -> ([Int], [Int])
    go [] _ _ = ([], [])
    go (Both _ _ : xs) !glold !glnew = bimap (glnew :) (glold :) $ go xs (glold + 1) (glnew + 1)
    go (First _ : xs) !glold !glnew = first (-1 :) $ go xs (glold + 1) glnew
    go (Second _ : xs) !glold !glnew = second (-1 :) $ go xs glold (glnew + 1)
-}
