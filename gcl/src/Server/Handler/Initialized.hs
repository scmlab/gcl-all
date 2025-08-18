{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Initialized (handler) where

import qualified Language.LSP.Server           as LSP
import qualified Language.LSP.Protocol.Types   as LSP
import qualified Language.LSP.Protocol.Message as LSP
import Server.Monad (ServerM)

handler :: ServerM ()
handler = do
  -- logText "initialized"
  let requestParams =
        LSP.ShowMessageRequestParams
          LSP.MessageType_Info
          "GCL Server Initialized."
          Nothing
  _ <- LSP.sendRequest LSP.SMethod_WindowShowMessageRequest requestParams $ \_ -> return ()
  return ()
