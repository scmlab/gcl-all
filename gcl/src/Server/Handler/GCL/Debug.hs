{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.GCL.Debug where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazy
import GHC.Generics (Generic)
import Server.Monad (ServerM, getFileState, logTextLn, readSource)
import qualified Server.ToClient as ToClient
import System.Directory (findExecutable)
import System.Environment (lookupEnv)

data DebugParams = DebugParams {filePath :: FilePath}
  deriving (Eq, Show, Generic)

instance JSON.FromJSON DebugParams

instance JSON.ToJSON DebugParams

handler :: DebugParams -> ServerM ()
handler DebugParams {filePath} = do
  -- Show the environment and executable SBV uses to locate Z3.
  -- SBV_Z3 overrides the default command name; otherwise SBV uses "z3".
  logTextLn ">>>> gcl.debug: PATH / SBV_Z3"
  mPath <- liftIO (lookupEnv "PATH")
  logTextLn $ Text.pack $ "PATH=" ++ maybe "(unset)" id mPath
  mSolver <- liftIO (lookupEnv "SBV_Z3")
  logTextLn $ Text.pack $ "SBV_Z3=" ++ maybe "(unset)" id mSolver
  let solver = maybe "z3" id mSolver
  mZ3 <- liftIO (findExecutable solver)
  logTextLn $ Text.pack $ "findExecutable " ++ show solver ++ " => " ++ maybe "NOT FOUND" id mZ3
  logTextLn "<<<< gcl.debug: PATH / SBV_Z3"

  logTextLn ">>>> gcl.debug: FileState"
  maybeFs <- getFileState filePath
  case maybeFs of
    Nothing -> logTextLn "  FileState not found"
    Just fs -> do
      let json = ToClient.toClientFileStateJSON filePath fs
      logTextLn . TextLazy.toStrict . TextLazy.decodeUtf8 . JSON.encode $ json
  logTextLn "<<<< gcl.debug: FileState"

  logTextLn ">>>> gcl.debug: source"
  maybeSource <- readSource filePath
  case maybeSource of
    Nothing -> do
      logTextLn $ Text.pack $ "source not found for filePath: " ++ filePath
    Just source -> do
      logTextLn source
  logTextLn "<<< gcl.debug: source"
