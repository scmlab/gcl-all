{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Initialized (handler) where

import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Server.Monad (ServerM, sendWindowShowMessage)

handler :: ServerM ()
handler = do
  -- logText "initialized"
  sendWindowShowMessage "GCL Server Initialized."
