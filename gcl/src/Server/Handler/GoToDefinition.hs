{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use forM_" #-}

module Server.Handler.GoToDefinition where

import qualified Language.LSP.Protocol.Types as LSP
import qualified Server.GoToDefn as GoToDefn
import qualified Server.IntervalMap as IntervalMap
import Server.Monad (FileState (..), ServerM, getFileState)
import qualified Server.SrcLoc as SrcLoc
import GCL.Range (posCol, posLine, mkPos)

-- | Convert OriginTargetRanges to LSP LocationLink
-- This is stage 3: convert internal ranges to LSP ranges and fill in the URI
originTargetRangesToLocationLink :: GoToDefn.OriginTargetRanges -> LSP.Uri -> LSP.LocationLink
originTargetRangesToLocationLink (GoToDefn.OriginTargetRanges originRange tgtRange tgtSelRange) uri =
  LSP.LocationLink
    { LSP._originSelectionRange = Just $ SrcLoc.toLSPRange originRange,
      LSP._targetUri = uri,
      LSP._targetRange = SrcLoc.toLSPRange tgtRange,
      LSP._targetSelectionRange = SrcLoc.toLSPRange tgtSelRange
    }

handler :: LSP.Uri -> LSP.Position -> ([LSP.LocationLink] -> ServerM ()) -> ServerM ()
handler uri lspPosition responder = do
  case LSP.uriToFilePath uri of
    Nothing -> responder []
    Just filePath -> do
      maybeFileState <- getFileState filePath
      case maybeFileState of
        Nothing -> responder []
        Just FileState {fsDefinitionLinks} -> do
          let pos = SrcLoc.fromLSPPosition lspPosition
          -- Try lookup at current position first
          case IntervalMap.lookup pos fsDefinitionLinks of
            Just otr ->
              responder [originTargetRangesToLocationLink otr uri]
            Nothing ->
              -- Fallback: try lookup at position - 1 (for cases like 'a^' where cursor is at the exclusive end)
              if posCol pos > 1
                then
                  let pos' = mkPos (posLine pos) (posCol pos - 1)
                   in case IntervalMap.lookup pos' fsDefinitionLinks of
                        Just otr -> responder [originTargetRangesToLocationLink otr uri]
                        Nothing -> responder []
                else responder []
