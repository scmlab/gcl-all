{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler.OnDidChangeTextDocument where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import qualified Language.LSP.Protocol.Types as LSP
import Numeric (showFFloat)
import Server.Change (mkLSPMoves)
import Server.Monad (FileState3 (..), PendingEdit (..), ServerM, deletePendingEdit, getFileState3, getPendingEdit, logText, readSource, setFileState3, translateFileState3)
import Server.Notification.Update (sendUpdateNotification3)

handler :: FilePath -> [LSP.TextDocumentContentChangeEvent] -> ServerM ()
handler filePath changes = do
  t0 <- liftIO getMonotonicTimeNSec
  maybePending <- getPendingEdit filePath
  case maybePending of
    Just PendingEdit {expectedContent, pendingFileState} -> do
      deletePendingEdit filePath
      maybeSource <- readSource filePath
      case maybeSource of
        Just src | src == expectedContent -> do
          setFileState3 filePath pendingFileState
          sendUpdateNotification3 filePath pendingFileState
        _ -> applyTranslation
    Nothing -> applyTranslation
  t1 <- liftIO getMonotonicTimeNSec
  let elapsedMs = fromIntegral (t1 - t0) / 1e6 :: Double
  logText $ "didChange: took " <> Text.pack (showFFloat (Just 3) elapsedMs "") <> " ms\n"
  where
    applyTranslation = do
      maybeFs3 <- getFileState3 filePath
      case maybeFs3 of
        Nothing -> return ()
        Just fs3 -> do
          let fs3' = translateFileState3 (mkLSPMoves changes) fs3
          setFileState3 filePath fs3'
          sendUpdateNotification3 filePath fs3'
