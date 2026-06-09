{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Server.Notification.Update where

import Data.Proxy (Proxy (Proxy))
import Server.Monad (FileState, ServerM, sendInlayHintRefresh, sendSemanticTokensRefresh, setFileState)
import qualified Server.Monad as Server
import qualified Server.ToClient as ToClient

setAndSendFileState :: FilePath -> FileState -> ServerM ()
setAndSendFileState filePath fs = do
  setFileState filePath fs
  let json = ToClient.toClientFileStateJSON filePath fs
  Server.sendCustomNotification (Proxy @"gcl/update") json

setAndSendFileStateWithRefresh :: FilePath -> FileState -> ServerM ()
setAndSendFileStateWithRefresh filePath fs = do
  setAndSendFileState filePath fs
  sendSemanticTokensRefresh
  sendInlayHintRefresh
