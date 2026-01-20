{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Handler.GCL.Reload where

import qualified Data.Aeson as JSON
import Error (Error (..))
import GHC.Generics (Generic)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as VFS
import Server.Load (LoadResponse (..), load)
import Server.Monad (ServerM)
import Server.Notification.Error (sendErrorNotification)
import Server.ToClient (ReloadResponse (..), toReloadResponse)

data ReloadParams = ReloadParams {filePath :: FilePath}
  deriving (Eq, Show, Generic)

instance JSON.FromJSON ReloadParams

instance JSON.ToJSON ReloadParams

handler :: ReloadParams -> (ReloadResponse -> ServerM ()) -> (ReloadResponse -> ServerM ()) -> ServerM ()
handler ReloadParams {filePath} onResult _ = do
  maybeVirtualFile <- LSP.getVirtualFile $ LSP.toNormalizedUri $ LSP.filePathToUri filePath
  case fmap VFS.virtualFileText maybeVirtualFile of
    Nothing -> do
      sendErrorNotification filePath [CannotReadFile filePath]
      onResult ReloadDone
    Just source -> do
      let vfsVersion = maybe 0 VFS.virtualFileVersion maybeVirtualFile
      response <- load filePath source
      case response of
        LoadDone -> onResult ReloadDone
        LoadNeedsEdit edits -> onResult $ toReloadResponse filePath vfsVersion edits
