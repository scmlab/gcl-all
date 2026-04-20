{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Convert error ranges from post-edit coordinates back to pre-edit coordinates.
--
-- After applying edits to source code and re-running analysis, errors are
-- reported in post-edit coordinates. This module converts them back to
-- pre-edit coordinates so they can be displayed on the original source.
--
-- When an error range touches an edited region, we "expand" it to cover
-- the entire original edit range, since the exact position within the
-- edited text has no meaningful original coordinate.
module Server.OrigCoord
  ( Edit (..),
    EditRegion (..),
    prepareEdits,
    convertRange,
    convertPos,
    ConvertSide (..),
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import GCL.Range (Pos (Pos), Range (Range), mkPos, mkRange, posCol, posLine)

-- | An edit in original coordinates: the range to be replaced, and the new text.
--   Edits must be non-overlapping and ordered by position.
data Edit = Edit
  { editRange :: !Range,
    editText :: !Text
  }
  deriving (Show)

-- | Pre-computed region info for one edit, with both original and new coordinates.
data EditRegion = EditRegion
  { -- | start of edited range in original coords
    erOrigStart :: !Pos,
    -- | end of edited range in original coords
    erOrigEnd :: !Pos,
    -- | start of edited range in new coords
    erNewStart :: !Pos,
    -- | end of edited range in new coords
    erNewEnd :: !Pos
  }
  deriving (Show, Eq)

-- | Which side of a range we are converting. Determines expansion behavior
--   when the position falls inside an edited region.
data ConvertSide
  = -- | Range start (inclusive): expand to original edit start
    SideStart
  | -- | Range end (exclusive): expand to original edit end,
    --   unless at the very first position of the edit (which means
    --   the range doesn't actually include any edited content)
    SideEnd
  deriving (Show, Eq)

-- | Prepare a list of edits into EditRegions with pre-computed new coordinates.
--
--   The key insight: the text between consecutive edits is unchanged, so we
--   can compute each edit's new-coordinate start by "walking" from the previous
--   edit's end.  The new-coordinate end is then determined by the replacement text.
--
--   The input edits must be in document order and non-overlapping (in original
--   coordinates).
prepareEdits :: [Edit] -> [EditRegion]
prepareEdits [] = []
prepareEdits (e0 : rest) =
  let er0 = mkFirstRegion e0
   in er0 : go er0 rest
  where
    mkFirstRegion (Edit (Range origS origE) text) =
      EditRegion origS origE origS (advancePos origS text)

    go _ [] = []
    go prev (Edit (Range origS origE) text : es) =
      let !newS = shiftAfter (erOrigEnd prev) (erNewEnd prev) origS
          !newE = advancePos newS text
          !er = EditRegion origS origE newS newE
       in er : go er es

    -- \| Given that @refFrom@ (in original coords) maps to @refTo@ (in new coords),
    --   compute the new coordinates of @pos@ (in original coords) which is in the
    --   unchanged text after the edit.
    shiftAfter :: Pos -> Pos -> Pos -> Pos
    shiftAfter refFrom refTo (Pos l c)
      | l == posLine refFrom =
          -- Same line as the edit end: column shift applies
          mkPos (posLine refTo) (posCol refTo + (c - posCol refFrom))
      | otherwise =
          -- Different line: only line shift, column unchanged
          mkPos (posLine refTo + (l - posLine refFrom)) c

    -- Advance a position by the content of the replacement text.
    -- If the text has no newlines, columns advance from the start column.
    -- If the text has newlines, the end column is the length of the last line + 1.
    advancePos :: Pos -> Text -> Pos
    advancePos (Pos sl sc) t =
      let !newLines = T.count "\n" t
       in if newLines == 0
            then mkPos sl (sc + T.length t)
            else mkPos (sl + newLines) (T.length (T.takeWhileEnd (/= '\n') t) + 1)

-- | Convert a single position from new coordinates to original coordinates.
--   Linear scan through the edit regions.
--
--   For positions in unchanged regions, we reverse the coordinate shift by
--   "walking back" from the nearest edit's end.
--   For positions inside an edited region, we expand to the original edit boundary.
convertPos :: ConvertSide -> [EditRegion] -> Pos -> Pos
convertPos side = go Nothing
  where
    go :: Maybe EditRegion -> [EditRegion] -> Pos -> Pos
    go prevEr [] pos = reverseShift prevEr pos
    go prevEr (er : ers) pos
      -- Position is before this edit region (in new coords): reverse shift
      | before side pos (erNewStart er) = reverseShift prevEr pos
      -- Position is inside this edit region (in new coords): expand
      | pos < erNewEnd er = expand side er
      -- Position is at or after edit region end: continue to next
      | otherwise = go (Just er) ers pos

    -- For Start (inclusive): pos < newStart means "before"
    -- For End (exclusive):   pos <= newStart means "before"
    --   (end-exclusive at newStart means the range doesn't include any edit content)
    before SideStart pos newStart = pos < newStart
    before SideEnd pos newStart = pos <= newStart

    -- Expand: when position is inside an edited region
    expand SideStart er = erOrigStart er
    expand SideEnd er = erOrigEnd er

    -- Reverse shift: map from new coords back to original coords.
    -- If no previous edit, the position is before any edit: unchanged.
    -- Otherwise, walk back from the previous edit's newEnd -> origEnd.
    reverseShift :: Maybe EditRegion -> Pos -> Pos
    reverseShift Nothing pos = pos
    reverseShift (Just er) (Pos l c)
      | l == posLine (erNewEnd er) =
          -- Same line as the edit's new-end: column shift applies
          mkPos (posLine (erOrigEnd er)) (posCol (erOrigEnd er) + (c - posCol (erNewEnd er)))
      | otherwise =
          -- Different line: only line shift, column unchanged
          mkPos (posLine (erOrigEnd er) + (l - posLine (erNewEnd er))) c

-- | Convert a Range from new coordinates to original coordinates.
convertRange :: [EditRegion] -> Range -> Range
convertRange ers (Range s e) =
  mkRange (convertPos SideStart ers s) (convertPos SideEnd ers e)
