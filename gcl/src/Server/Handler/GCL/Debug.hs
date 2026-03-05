{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.GCL.Debug where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazy
import GHC.Generics (Generic)
import Server.Monad (ServerM, getFileState3, logTextLn, readSource)
import qualified Server.ToClient as ToClient

data DebugParams = DebugParams {filePath :: FilePath}
  deriving (Eq, Show, Generic)

instance JSON.FromJSON DebugParams

instance JSON.ToJSON DebugParams

handler :: DebugParams -> (() -> ServerM ()) -> (() -> ServerM ()) -> ServerM ()
handler DebugParams {filePath} onResult _ = do
  logTextLn ">>>> gcl.debug: FileState3"
  maybeFs3 <- getFileState3 filePath
  case maybeFs3 of
    Nothing -> logTextLn "  FileState3 not found"
    Just fs3 -> do
      let json = ToClient.toFileState3NotificationJSON filePath fs3
      logTextLn . TextLazy.toStrict . TextLazy.decodeUtf8 . JSON.encode $ json
  logTextLn "<<<< gcl.debug: FileState3"

  logTextLn ">>>> gcl.debug: source"
  maybeSource <- readSource filePath
  case maybeSource of
    Nothing -> do
      logTextLn $ Text.pack $ "source not found for filePath: " ++ filePath
    Just source -> do
      logTextLn source
  logTextLn "<<< gcl.debug: source"

  onResult ()
