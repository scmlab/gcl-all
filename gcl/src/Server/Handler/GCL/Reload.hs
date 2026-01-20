{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Handler.GCL.Reload where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as VFS
import Server.Load (LoadResponse (..), load)
import Server.Monad (ServerM)
import Server.ToClient (ReloadResponse (..), toReloadResponse)

data ReloadParams = ReloadParams {filePath :: FilePath}
  deriving (Eq, Show, Generic)

instance JSON.FromJSON ReloadParams

instance JSON.ToJSON ReloadParams

handler :: ReloadParams -> (ReloadResponse -> ServerM ()) -> (ReloadResponse -> ServerM ()) -> ServerM ()
handler ReloadParams {filePath} onResult _ = do
  response <- load filePath
  case response of
    LoadDone -> onResult ReloadDone
    LoadNeedsEdit edits -> do
      maybeVirtualFile <- LSP.getVirtualFile $ LSP.toNormalizedUri $ LSP.filePathToUri filePath
      let vfsVersion = maybe 0 VFS.virtualFileVersion maybeVirtualFile
      onResult $ toReloadResponse filePath vfsVersion edits
