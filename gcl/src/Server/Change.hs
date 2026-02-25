{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Change
  ( LSPMove (..),
    mkLSPMove,
    mkLSPMoves,
    applyLSPMove,
    applyLSPMovesToToken,
    GCLMove,
    fromLSPMove,
    applyGCLMove,
    applyMovesToIntervalMap,
    updateOriginTargetRanges,
  )
where

import Control.Monad (foldM)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import GCL.Range (Pos (Pos), Range (Range), mkPos, mkRange, posLine, posOrd, rangeEnd, rangeStart)
import qualified Hack
import qualified Language.LSP.Protocol.Types as LSP
import Server.GoToDefn (OriginTargetRanges (..))
import Server.IntervalMap (IntervalMap)
import qualified Server.IntervalMap as IntervalMap
import Server.SrcLoc (fromLSPRange)

-- | Pre-computed info for moving existing LSP Ranges after an edit.
--   Contains only positional displacement, not the replacement text.
--
--   start, end = changed range (before edit, 0-based LSP.Position)
--   dL = new lines introduced minus old lines removed
--   dC = column shift for positions on the same line as the change end
data LSPMove = LSPMove
  { start :: !LSP.Position,
    end :: !LSP.Position,
    dL :: !Int,
    dC :: !Int
  }
  deriving (Eq, Show)

-- | Build an LSPMove from an LSP.Range and replacement text.
--   The text is only used to compute the delta; it is not stored.
mkLSPMove :: LSP.Range -> T.Text -> LSPMove
mkLSPMove (LSP.Range s@(LSP.Position sl sc) e@(LSP.Position el ec)) text =
  let !sC_ = fromIntegral sc
      !eC_ = fromIntegral ec
      !linesOld = fromIntegral el - fromIntegral sl :: Int
      !linesNew = T.count "\n" text
      !lineDiff = linesNew - linesOld
      !newEndC
        | linesNew == 0 = sC_ + T.length text
        | otherwise = T.length (T.takeWhileEnd (/= '\n') text)
      !cDiff = newEndC - eC_
   in LSPMove
        { start = s,
          end = e,
          dL = lineDiff,
          dC = cDiff
        }

-- | Convert a list of TextDocumentContentChangeEvents to LSPMoves.
--   Errors on non-incremental (whole-document) changes, which should never
--   occur since the server only declares TextDocumentSyncKind_Incremental.
mkLSPMoves :: [LSP.TextDocumentContentChangeEvent] -> [LSPMove]
mkLSPMoves = map go
  where
    go (LSP.TextDocumentContentChangeEvent (LSP.InL (LSP.TextDocumentContentChangePartial range _ text))) =
      mkLSPMove range text
    go _ = error "mkLSPMoves: received full document change, but server only declared incremental sync"

-- | Apply an LSPMove to an LSP.Range (end-exclusive).
--
--   Before change: unchanged.
--   After  change: shifted by (dL, dC).
--   Overlapping:   invalidated (Nothing).
applyLSPMove :: LSP.Range -> LSPMove -> Maybe LSP.Range
applyLSPMove (LSP.Range oS oE) (LSPMove s e dL dC)
  -- range ends before change starts: unchanged
  | oE <= s = Just (LSP.Range oS oE)
  -- range starts at or after change end: shift
  | oS >= e =
      let !nS = shiftPos oS
          !nE = shiftPos oE
       in Just (LSP.Range nS nE)
  -- overlap: invalidated
  | otherwise = Nothing
  where
    LSP.Position eL _ = e
    shiftPos (LSP.Position l c) =
      let !nL = fromIntegral l + dL
          !nC = if l == eL then fromIntegral c + dC else fromIntegral c
       in LSP.Position (Hack.intToUInt nL) (Hack.intToUInt nC)

--------------------------------------------------------------------------------
-- GCLMove: same logic, GCL coordinates (1-based)
--------------------------------------------------------------------------------

-- | GCL-coordinate version of LSPMove.
--   Positional: start end dL dC
data GCLMove = GCLMove !Pos !Pos !Int !Int
  deriving (Eq, Show)

-- | Convert an LSPMove (0-based) to a GCLMove (1-based).
--   dL and dC are coordinate-system-independent.
fromLSPMove :: LSPMove -> GCLMove
fromLSPMove (LSPMove s e dl dc) = GCLMove gclS gclE dl dc
  where
    Range gclS gclE = fromLSPRange (LSP.Range s e)

-- | Apply a GCLMove to a GCL Range (end-exclusive).
--   Before change: unchanged.  After change: shifted.  Overlapping: Nothing.
applyGCLMove :: Range -> GCLMove -> Maybe Range
applyGCLMove (Range oS oE) (GCLMove s e dL dC)
  | oE <= s = Just (mkRange oS oE)
  | oS >= e =
      let !nS = shiftPos oS
          !nE = shiftPos oE
       in Just (mkRange nS nE)
  | otherwise = Nothing
  where
    eL = posLine e
    shiftPos (Pos l c) =
      let !nL = l + dL
          !nC = if l == eL then c + dC else c
       in mkPos nL nC

-- | Apply a list of GCLMoves to the three GCL Ranges inside an OriginTargetRanges.
--   Returns Nothing if any range is invalidated by the moves.
updateOriginTargetRanges :: [GCLMove] -> OriginTargetRanges -> Maybe OriginTargetRanges
updateOriginTargetRanges moves (OriginTargetRanges orig tgt tgtSel) = do
  orig' <- foldM applyGCLMove orig moves
  tgt' <- foldM applyGCLMove tgt moves
  tgtSel' <- foldM applyGCLMove tgtSel moves
  return (OriginTargetRanges orig' tgt' tgtSel')

--------------------------------------------------------------------------------
-- IntervalMap helpers
--------------------------------------------------------------------------------

-- | Reconstruct a GCL Range from two posOrd integers.
posOrdToRange :: (Int, Int) -> Range
posOrdToRange (s, e) =
  let (sLine, sCol) = s `divMod` 10000000
      (eLine, eCol) = e `divMod` 10000000
   in mkRange (mkPos sLine sCol) (mkPos eLine eCol)

-- | Apply a list of GCLMoves to every entry in an IntervalMap.
--   The key range and the payload are both updated via the supplied function.
--   Any entry whose key range or payload update returns Nothing is dropped.
applyMovesToIntervalMap :: forall a. [GCLMove] -> ([GCLMove] -> a -> Maybe a) -> IntervalMap a -> IntervalMap a
applyMovesToIntervalMap moves updatePayload imap =
  IntervalMap.fromAscList $ mapMaybe applyEntry (IntervalMap.toList imap)
  where
    applyEntry :: ((Int, Int), a) -> Maybe ((Int, Int), a)
    applyEntry (ords, value) = do
      newRange <- foldM applyGCLMove (posOrdToRange ords) moves
      newValue <- updatePayload moves value
      return
        ( (posOrd (rangeStart newRange), posOrd (rangeEnd newRange)),
          newValue
        )

--------------------------------------------------------------------------------
-- SemanticToken helpers
--------------------------------------------------------------------------------

-- | Apply a list of LSPMoves to a single SemanticTokenAbsolute.
--   The token is converted to a Range, moved through all changes, then converted back.
applyLSPMovesToToken :: [LSPMove] -> LSP.SemanticTokenAbsolute -> Maybe LSP.SemanticTokenAbsolute
applyLSPMovesToToken moves (LSP.SemanticTokenAbsolute tLine tChar tLen tType tMods) = do
  let tokenRange = LSP.Range (LSP.Position tLine tChar) (LSP.Position tLine (tChar + tLen))
  LSP.Range (LSP.Position nL nC) _ <- foldM applyLSPMove tokenRange moves
  Just (LSP.SemanticTokenAbsolute nL nC tLen tType tMods)
