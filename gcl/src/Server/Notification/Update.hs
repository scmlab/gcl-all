{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Server.Notification.Update where

import Data.Proxy (Proxy (Proxy))
import Server.Monad (FileState3, ServerM)
import qualified Server.Monad as Server
import qualified Server.ToClient as ToClient

sendUpdateNotification3 :: FilePath -> FileState3 -> ServerM ()
sendUpdateNotification3 filePath fs3 = do
  let json = ToClient.toFileState3NotificationJSON filePath fs3
  Server.sendCustomNotification (Proxy @"gcl/update") json
