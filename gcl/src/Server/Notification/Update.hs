{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Server.Notification.Update where

import qualified Data.Aeson as JSON
import Data.Proxy (Proxy (Proxy))
import Server.Monad (ServerM, loadFileState)
import qualified Server.Monad as Server
import qualified Server.ToClient as ToClient

sendUpdateNotification :: FilePath -> ServerM ()
sendUpdateNotification filePath = do
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> return ()
    Just fileState -> do
      let clientFileState = ToClient.convertFileState filePath fileState
      let json = JSON.toJSON clientFileState
      Server.sendCustomNotification (Proxy @"gcl/update") json
