-- module for LSP <=> GCL srcloc convertion

module Server.SrcLoc
  ( fromLSPRange,
    fromLSPPosition,
    toLSPRange,
    toLSPPosition,
  )
where

import GCL.Range (Pos (..), Range (..), mkPos, mkRange)
import qualified Hack
import qualified Language.LSP.Protocol.Types as J

-- Note:  LSP srclocs are 0-base
--        Data.Loc/Data.Range srclocs are 1-base

--------------------------------------------------------------------------------

-- | LSP source locations => Data.Loc/Data.Range source locations

-- | LSP Range -> Data.Range.Range
fromLSPRange :: J.Range -> Range
fromLSPRange (J.Range start end) =
  mkRange
    (fromLSPPosition start)
    (fromLSPPosition end)

-- | LSP Position -> GCL.Range.Pos
fromLSPPosition :: J.Position -> Pos
fromLSPPosition (J.Position line col) =
  mkPos
    (fromIntegral line + 1) -- starts at 1
    (fromIntegral col + 1) -- starts at 1

toLSPRange :: Range -> J.Range
toLSPRange (Range start end) = J.Range (toLSPPosition start) (toLSPPosition end)

toLSPPosition :: Pos -> J.Position
toLSPPosition (Pos ln col) = J.Position ((Hack.intToUInt (ln - 1)) `max` 0) ((Hack.intToUInt (col - 1)) `max` 0)
