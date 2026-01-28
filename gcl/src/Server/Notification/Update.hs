{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Server.Notification.Update where

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
      let json = ToClient.toFileStateNotificationJSON filePath fileState
      Server.sendCustomNotification (Proxy @"gcl/update") json
      -- Request client to refresh semantic tokens now that file state is ready
      Server.sendSemanticTokensRefresh
