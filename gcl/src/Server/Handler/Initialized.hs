{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Initialized (handler) where

import Server.Monad (ServerM, sendWindowShowMessage)

handler :: ServerM ()
handler = do
  -- logText "initialized"
  sendWindowShowMessage "GCL Server Initialized."
