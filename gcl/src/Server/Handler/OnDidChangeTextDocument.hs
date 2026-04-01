{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler.OnDidChangeTextDocument where

import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import qualified Language.LSP.Protocol.Types as LSP
import Numeric (showFFloat)
import Server.Monad (FileState (..), PendingEdit (..), ServerM, deletePendingEdit, getFileState, getPendingEdit, logText, readSource, setFileState)
import Server.Move (applyMovesToFileState, mkLSPMoves)
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
          let fs' = applyMovesToFileState (mkLSPMoves changes) fs
          -- Force all list fields now so the work is done within this handler,
          -- rather than deferred lazily to the next request.
          (nToks, nSpecs, nHoles, nPOs, nWarnings) <- liftIO $ do
            a <- evaluate $ length $ fsSemanticTokens fs'
            b <- evaluate $ length $ fsSpecifications fs'
            c <- evaluate $ length $ fsHoles fs'
            d <- evaluate $ length $ fsProofObligations fs'
            e <- evaluate $ length $ fsWarnings fs'
            return (a, b, c, d, e)
          logText $
            "tokens: "
              <> Text.pack (show nToks)
              <> " specs: "
              <> Text.pack (show nSpecs)
              <> " holes: "
              <> Text.pack (show nHoles)
              <> " POs: "
              <> Text.pack (show nPOs)
              <> " warnings: "
              <> Text.pack (show nWarnings)
              <> "\n"
          setFileState filePath fs'
          sendUpdateNotification filePath fs'
