{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use forM_" #-}

module Server.Handler.GoToDefinition where

import qualified Language.LSP.Protocol.Types as LSP
import qualified Server.GoToDefn as GoToDefn
import qualified Server.IntervalMap as IntervalMap
import Server.Monad (FileState (..), ServerM, loadFileState)
import qualified Server.SrcLoc as SrcLoc

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
      maybeFileState <- loadFileState filePath
      case maybeFileState of
        Nothing -> responder []
        Just
          FileState
            { definitionLinks,
              toOffsetMap
            } -> do
            let pos = SrcLoc.fromLSPPosition toOffsetMap lspPosition
            case IntervalMap.lookup pos definitionLinks of
              Nothing -> responder []
              Just otr -> responder [originTargetRangesToLocationLink otr uri]
