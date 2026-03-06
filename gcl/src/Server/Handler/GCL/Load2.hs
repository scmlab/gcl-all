{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.GCL.Load2 where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Aeson.Types as JSON
import Data.Int (Int32)
import Data.Text (Text)
import Error (Error)
import GHC.Generics (Generic)
import Server.Load2 (DigResult, loadAndDig)
import Server.Monad (FileState3, PendingEdit (..), ServerM, editTextsWithVersion, logText, logTextLn, readSourceAndVersion, setFileState3, setPendingEdit)
import Server.Notification.Error (sendErrorNotification)
import Server.Notification.Update (sendUpdateNotification3)

data ReloadParams = ReloadParams {filePath :: FilePath}
  deriving (Eq, Show, Generic)

instance JSON.FromJSON ReloadParams

instance JSON.ToJSON ReloadParams

handler :: ReloadParams -> (() -> ServerM ()) -> (() -> ServerM ()) -> ServerM ()
handler ReloadParams {filePath} onResult _ = do
  logText "Load2: start\n"
  result <- runExceptT $ do
    (source, vfsVersion) <- readSourceOrThrow filePath
    (maybeDig, fs3) <- loadOrThrow filePath source
    return (vfsVersion, maybeDig, fs3)
  case result of
    Left errs -> do
      sendErrorNotification filePath errs
      onResult ()
    Right (vfsVersion, maybeDig, fs3) -> do
      case maybeDig of
        Nothing -> do
          logText "Load2: no holes, saving directly\n"
          setFileState3 filePath fs3
          sendUpdateNotification3 filePath fs3
        Just (edits, newSource) -> do
          logText "Load2: holes dug, setting pending edit\n"
          let pending =
                PendingEdit
                  { expectedContent = newSource,
                    pendingFileState = fs3
                  }
          setPendingEdit filePath pending
          editTextsWithVersion filePath vfsVersion edits
      onResult ()
  logText "Load2: end\n"

readSourceOrThrow :: FilePath -> ExceptT [Error] ServerM (Text, Int32)
readSourceOrThrow filePath = do
  lift $ logText "Load2: reading virtual file\n"
  result <- lift $ readSourceAndVersion filePath
  case result of
    Nothing -> do
      lift $ logText "Load2: cannot read virtual file\n"
      throwE []
    Just x -> return x

loadOrThrow :: FilePath -> Text -> ExceptT [Error] ServerM (Maybe DigResult, FileState3)
loadOrThrow filePath source = do
  case loadAndDig filePath source of
    Left err -> do
      lift $ logTextLn "Load2: load error"
      throwE [err]
    Right result -> return result
