{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.GCL.Debug where

import qualified Data.Aeson.Types as JSON
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Server.Monad (ServerM, loadFileState, logTextLn, readSource)

data DebugParams = DebugParams {filePath :: FilePath}
  deriving (Eq, Show, Generic)

instance JSON.FromJSON DebugParams

instance JSON.ToJSON DebugParams

handler :: DebugParams -> (() -> ServerM ()) -> (() -> ServerM ()) -> ServerM ()
handler DebugParams {filePath} onResult _ = do
  logTextLn ">>>> gcl.debug: FileState"
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> logTextLn "  FileState not found"
    Just fileState -> logTextLn . Text.pack . show $ fileState
  logTextLn "<<<< gcl.debug: FileState"

  logTextLn ">>>> gcl.debug: source"
  maybeSource <- readSource filePath
  case maybeSource of
    Nothing -> do
      logTextLn $ Text.pack $ "source not found for filePath: " ++ filePath
    Just source -> do
      logTextLn source
  logTextLn "<<< gcl.debug: source"

  onResult ()
