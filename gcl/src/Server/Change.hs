{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Change
  ( LSPMove (..),
    mkLSPMove,
    mkLSPMoves,
    applyLSPMove,
    applyLSPMovesToToken,
  )
where

import Control.Monad (foldM)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Hack
import qualified Language.LSP.Protocol.Types as LSP

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
--   Skips non-incremental (whole-document) changes.
mkLSPMoves :: [LSP.TextDocumentContentChangeEvent] -> [LSPMove]
mkLSPMoves = mapMaybe go
  where
    go (LSP.TextDocumentContentChangeEvent (LSP.InL (LSP.TextDocumentContentChangePartial range _ text))) =
      Just (mkLSPMove range text)
    go _ = Nothing

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

-- | Apply a list of LSPMoves to a single SemanticTokenAbsolute.
--   The token is converted to a Range, moved through all changes, then converted back.
applyLSPMovesToToken :: [LSPMove] -> LSP.SemanticTokenAbsolute -> Maybe LSP.SemanticTokenAbsolute
applyLSPMovesToToken moves (LSP.SemanticTokenAbsolute tLine tChar tLen tType tMods) = do
  let tokenRange = LSP.Range (LSP.Position tLine tChar) (LSP.Position tLine (tChar + tLen))
  LSP.Range (LSP.Position nL nC) _ <- foldM applyLSPMove tokenRange moves
  Just (LSP.SemanticTokenAbsolute nL nC tLen tType tMods)
