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
import Server.Monad (FileState (..), PendingEdit (..), ServerM, deletePendingEdit, getFileState, getPendingEdit, logText, readSource, setFileState, translateFileState)
import Server.Notification.Update (sendUpdateNotification)

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
          setFileState filePath pendingFileState
          sendUpdateNotification filePath pendingFileState
        _ -> applyTranslation
    Nothing -> applyTranslation
  t1 <- liftIO getMonotonicTimeNSec
  let elapsedMs = fromIntegral (t1 - t0) / 1e6 :: Double
  logText $ "didChange: took " <> Text.pack (showFFloat (Just 3) elapsedMs "") <> " ms\n"
  where
    applyTranslation = do
      maybeFs <- getFileState filePath
      case maybeFs of
        Nothing -> return ()
        Just fs -> do
          let fs' = translateFileState (mkLSPMoves changes) fs
          setFileState filePath fs'
          sendUpdateNotification filePath fs'
