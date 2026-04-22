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
  ( EditRegion (..),
    prepareEdits,
    convertRange,
    convertPos,
    ConvertSide (..),
    convertError,
  )
where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Error (Error (..))
import GCL.Range (Pos (Pos), Range (Range), mkPos, mkRange, posCol, posLine, rangeStart)
import GCL.Type (TypeError (..))
import GCL.WP.Types (StructError (..))
import Syntax.Common.Types (Name (..))
import Syntax.Parser.Error (ParseError (..))

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
--   The input edits must be non-overlapping (in original coordinates).
--   They will be sorted by start position internally.
prepareEdits :: [(Range, Text)] -> [EditRegion]
prepareEdits edits = case sortBy (comparing (rangeStart . fst)) edits of
  [] -> []
  (e0 : rest) ->
    let er0 = mkFirstRegion e0
     in er0 : go er0 rest
  where
    mkFirstRegion (Range origS origE, text) =
      EditRegion origS origE origS (advancePos origS text)

    go _ [] = []
    go prev ((Range origS origE, text) : es) =
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

--------------------------------------------------------------------------------
-- Converting Error ranges from post-edit to pre-edit coordinates

convertMaybeRange :: [EditRegion] -> Maybe Range -> Maybe Range
convertMaybeRange ers = fmap (convertRange ers)

convertName :: [EditRegion] -> Name -> Name
convertName ers (Name text mr) = Name text (convertMaybeRange ers mr)

convertParseError :: [EditRegion] -> ParseError -> ParseError
convertParseError ers (LexicalError pos) = LexicalError (convertPos SideStart ers pos)
convertParseError ers (SyntacticError errs logMsg) =
  SyntacticError (fmap (\(mr, s) -> (convertMaybeRange ers mr, s)) errs) logMsg

convertTypeError :: [EditRegion] -> TypeError -> TypeError
convertTypeError ers (NotInScope n) = NotInScope (convertName ers n)
convertTypeError ers (UnifyFailed t1 t2 mr) = UnifyFailed t1 t2 (convertMaybeRange ers mr)
convertTypeError ers (KindUnifyFailed k1 k2 mr) = KindUnifyFailed k1 k2 (convertMaybeRange ers mr)
convertTypeError ers (RecursiveType n t mr) = RecursiveType n t (convertMaybeRange ers mr)
convertTypeError ers (AssignToConst n) = AssignToConst (convertName ers n)
convertTypeError ers (UndefinedType n) = UndefinedType (convertName ers n)
convertTypeError ers (DuplicatedIdentifiers ns) = DuplicatedIdentifiers (map (convertName ers) ns)
convertTypeError ers (RedundantNames ns) = RedundantNames (map (convertName ers) ns)
convertTypeError _ (RedundantExprs exprs) = RedundantExprs exprs
convertTypeError ers (MissingArguments ns) = MissingArguments (map (convertName ers) ns)
convertTypeError ers (PatternArityMismatch expected actual mr) =
  PatternArityMismatch expected actual (convertMaybeRange ers mr)

convertStructError :: [EditRegion] -> StructError -> StructError
convertStructError ers (MissingAssertion mr) = MissingAssertion (convertMaybeRange ers mr)
convertStructError ers (MissingPostcondition mr) = MissingPostcondition (convertMaybeRange ers mr)
convertStructError ers (MultiDimArrayAsgnNotImp mr) = MultiDimArrayAsgnNotImp (convertMaybeRange ers mr)
convertStructError ers (LocalVarExceedScope mr) = LocalVarExceedScope (convertMaybeRange ers mr)

-- | Convert all ranges in an Error from post-edit to pre-edit coordinates.
convertError :: [EditRegion] -> Error -> Error
convertError ers (ParseError pe) = ParseError (convertParseError ers pe)
convertError ers (TypeError te) = TypeError (convertTypeError ers te)
convertError ers (StructError se) = StructError (convertStructError ers se)
convertError ers (Others title msg mr) = Others title msg (convertMaybeRange ers mr)
convertError _ e = e
