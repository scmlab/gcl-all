{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Change
  ( LSPMove (..),
    mkLSPMove,
    applyLSPMove,
  )
where

import qualified Data.Text as T
import qualified Hack
import Language.LSP.Protocol.Types
  ( Position (Position),
  )

-- | Pre-computed info for moving existing LSP Ranges after an edit.
--   Contains only positional displacement, not the replacement text.
--
--   start, end = changed range (before edit, 0-based LSP Position)
--   dL = new lines introduced minus old lines removed
--   dC = column shift for positions on the same line as the change end
data LSPMove = LSPMove
  { start :: !Position,
    end :: !Position,
    dL :: !Int,
    dC :: !Int
  }
  deriving (Eq, Show)

-- | Build an LSPMove from two LSP Positions (0-based) and replacement text.
--   The text is only used to compute the delta; it is not stored.
mkLSPMove :: Position -> Position -> T.Text -> LSPMove
mkLSPMove s@(Position sl sc) e@(Position el ec) text =
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

-- | Apply an LSPMove to an LSP Range (two Positions, end-exclusive).
--
--   Before change: unchanged.
--   After  change: shifted by (dL, dC).
--   Overlapping:   invalidated (Nothing).
applyLSPMove :: Position -> Position -> LSPMove -> Maybe (Position, Position)
applyLSPMove oS oE (LSPMove s e dL dC)
  -- range ends before change starts: unchanged
  | oE <= s = Just (oS, oE)
  -- range starts at or after change end: shift
  | oS >= e =
      let !nS = shiftPos oS
          !nE = shiftPos oE
       in Just (nS, nE)
  -- overlap: invalidated
  | otherwise = Nothing
  where
    Position eL _ = e
    shiftPos (Position l c) =
      let !nL = fromIntegral l + dL
          !nC = if l == eL then fromIntegral c + dC else fromIntegral c
       in Position (Hack.intToUInt nL) (Hack.intToUInt nC)
