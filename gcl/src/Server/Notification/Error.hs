{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Server.Notification.Error where

import Data.Proxy (Proxy (Proxy))
import Error (Error)
import Server.Monad (ServerM)
import qualified Server.Monad as Server
import Server.ToClient (toErrorNotificationJSON)

sendErrorNotification :: FilePath -> [Error] -> ServerM ()
sendErrorNotification filePath errors = do
  let json = toErrorNotificationJSON filePath errors
  Server.sendCustomNotification (Proxy @"gcl/error") json
