{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Server.Notification.Update where

import Data.Proxy (Proxy (Proxy))
import qualified Language.LSP.Protocol.Types as LSP
import Server.Monad (FileState, ServerM, sendInlayHintRefresh, sendSemanticTokensRefresh, setFileState)
import qualified Server.Monad as Server
import qualified Server.ToClient as ToClient

setAndSendFileState :: FilePath -> Maybe LSP.Int32 -> FileState -> ServerM ()
setAndSendFileState filePath loadDocumentVersion fs = do
  setFileState filePath fs
  let json = ToClient.toClientFileStateJSON filePath loadDocumentVersion fs
  Server.sendCustomNotification (Proxy @"gcl/update") json

setAndSendFileStateWithRefresh :: FilePath -> Maybe LSP.Int32 -> FileState -> ServerM ()
setAndSendFileStateWithRefresh filePath loadDocumentVersion fs = do
  setAndSendFileState filePath loadDocumentVersion fs
  sendSemanticTokensRefresh
  sendInlayHintRefresh
