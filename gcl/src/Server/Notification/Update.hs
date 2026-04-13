{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Server.Notification.Update where

import Data.Proxy (Proxy (Proxy))
import Server.Monad (FileState, ServerM, sendInlayHintRefresh, sendSemanticTokensRefresh)
import qualified Server.Monad as Server
import qualified Server.ToClient as ToClient

sendFileState :: FilePath -> FileState -> ServerM ()
sendFileState filePath fs = do
  let json = ToClient.toClientFileStateJSON filePath fs
  Server.sendCustomNotification (Proxy @"gcl/update") json

sendFileStateWithRefresh :: FilePath -> FileState -> ServerM ()
sendFileStateWithRefresh filePath fs = do
  sendFileState filePath fs
  sendSemanticTokensRefresh
  sendInlayHintRefresh
