{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server.Monad
  ( module Server.FileState,
    GlobalState (..),
    ServerM,
    initGlobalEnv,
    runServerM,
    runServerMLogError,
    logText,
    logTextLn,
    getFileState,
    setFileState,
    deleteFileState,
    getPendingEdit,
    setPendingEdit,
    deletePendingEdit,
    readSource,
    readSourceAndVersion,
    sendEditTextsWithVersion,
    sendCustomNotification,
    sendWindowShowMessage,
    sendWindowInfoMessage,
    sendSemanticTokensRefresh,
    sendInlayHintRefresh,
  )
where

import Control.Concurrent
  ( Chan,
    newChan,
    threadDelay,
    writeChan,
  )
import Control.Exception (SomeException, catch, displayException, throwIO)
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import Data.IORef
  ( IORef,
    modifyIORef,
    newIORef,
    readIORef,
  )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Range (Range)
import GHC.TypeLits (KnownSymbol)
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as LSP
import Server.FileState
import qualified Server.SrcLoc as SrcLoc

-- | State shared by all clients and requests
data GlobalState = GlobalState
  { logChannel :: Chan Text, -- Channel for printing log
    filesState :: IORef (Map FilePath FileState),
    pendingEdits :: IORef (Map FilePath PendingEdit)
  }

-- | Constructs an initial global state
initGlobalEnv :: IO GlobalState
initGlobalEnv =
  GlobalState
    <$> newChan
    <*> newIORef Map.empty
    <*> newIORef Map.empty

--------------------------------------------------------------------------------

type ServerM = LSP.LspT () (ReaderT GlobalState IO)

runServerM :: GlobalState -> LSP.LanguageContextEnv () -> ServerM a -> IO a
runServerM globalState ctxEnv program = runReaderT (LSP.runLspT ctxEnv program) globalState

-- Wrap runServerM with error logging
runServerMLogError :: GlobalState -> LSP.LanguageContextEnv () -> ServerM a -> IO a
runServerMLogError globalState ctxEnv program =
  catch (runServerM globalState ctxEnv program) (handleException globalState)
  where
    handleException :: GlobalState -> SomeException -> IO a
    handleException gs e = do
      let errorMsg = "\n========== FATAL ERROR in handler ==========\n" ++ displayException e ++ "\n"
      appendFile "gcl_crash.log" errorMsg
      writeChan (logChannel gs) (Text.pack errorMsg)
      threadDelay 500000 -- sleep 0.5 sec (best effort)
      throwIO e

--------------------------------------------------------------------------------

-- | Helper functions for side effects

-- display Text
logText :: Text -> ServerM ()
logText s = do
  chan <- lift $ asks logChannel
  liftIO $ writeChan chan s

logTextLn :: Text -> ServerM ()
logTextLn s = logText (s <> "\n")

getFileState :: FilePath -> ServerM (Maybe FileState)
getFileState filePath = do
  ref <- lift $ asks filesState
  m <- liftIO $ readIORef ref
  return $ Map.lookup filePath m

setFileState :: FilePath -> FileState -> ServerM ()
setFileState filePath fs = do
  ref <- lift $ asks filesState
  liftIO $ modifyIORef ref (Map.insert filePath fs)

deleteFileState :: FilePath -> ServerM ()
deleteFileState filePath = do
  ref <- lift $ asks filesState
  liftIO $ modifyIORef ref (Map.delete filePath)

getPendingEdit :: FilePath -> ServerM (Maybe PendingEdit)
getPendingEdit filePath = do
  ref <- lift $ asks pendingEdits
  m <- liftIO $ readIORef ref
  return $ Map.lookup filePath m

setPendingEdit :: FilePath -> PendingEdit -> ServerM ()
setPendingEdit filePath pe = do
  ref <- lift $ asks pendingEdits
  existing <- liftIO $ Map.lookup filePath <$> readIORef ref
  case existing of
    Just _ ->
      error $
        "setPendingEdit: invariant violated for "
          <> filePath
          <> "\n"
          <> "A pending edit already exists for this file, meaning a previous workspace/applyEdit\n"
          <> "was sent but its corresponding didChange has not yet been received.\n"
          <> "Setting a second pending edit would be wrong: the new edit corresponds to a\n"
          <> "workspace/applyEdit that will fail (version mismatch), so its expectedContent\n"
          <> "would never match, corrupting the pending edit state."
    Nothing ->
      liftIO $ modifyIORef ref (Map.insert filePath pe)

deletePendingEdit :: FilePath -> ServerM ()
deletePendingEdit filePath = do
  ref <- lift $ asks pendingEdits
  liftIO $ modifyIORef ref (Map.delete filePath)

readSource :: FilePath -> ServerM (Maybe Text)
readSource filepath = do
  maybeVirtualFile <- LSP.getVirtualFile $ LSP.toNormalizedUri $ LSP.filePathToUri filepath
  case maybeVirtualFile of
    Nothing -> return Nothing
    Just virtualFile -> do
      logTextLn $ "readSource: LSP.virtualFileVersion: " <> Text.pack (Prelude.show $ LSP.virtualFileVersion virtualFile)
      return (Just $ LSP.virtualFileText virtualFile)

readSourceAndVersion :: FilePath -> ServerM (Maybe (Text, LSP.Int32))
readSourceAndVersion filepath = do
  maybeVirtualFile <- LSP.getVirtualFile $ LSP.toNormalizedUri $ LSP.filePathToUri filepath
  case maybeVirtualFile of
    Nothing -> return Nothing
    Just virtualFile ->
      return (Just (LSP.virtualFileText virtualFile, LSP.virtualFileVersion virtualFile))

-- | Send edits to client with a specific document version.
-- The client will reject the edit if the version doesn't match.
sendEditTextsWithVersion :: FilePath -> LSP.Int32 -> [(Range, Text)] -> ServerM ()
sendEditTextsWithVersion filepath version rangeTextPairs = do
  let requestParams =
        LSP.ApplyWorkspaceEditParams
          { _label = Just "GCL Edit",
            _edit =
              LSP.WorkspaceEdit
                { _changes = Nothing,
                  _documentChanges = Just [LSP.InL textDocumentEdit],
                  _changeAnnotations = Nothing
                }
          }
  _requestId <- LSP.sendRequest LSP.SMethod_WorkspaceApplyEdit requestParams (\_response -> return ())
  return ()
  where
    textDocumentEdit :: LSP.TextDocumentEdit
    textDocumentEdit =
      LSP.TextDocumentEdit
        { _textDocument = LSP.OptionalVersionedTextDocumentIdentifier (LSP.filePathToUri filepath) (LSP.InL version),
          _edits = Prelude.map LSP.InL textEdits
        }
    textEdits :: [LSP.TextEdit]
    textEdits = Prelude.map makeTextEdit rangeTextPairs
    makeTextEdit :: (Range, Text) -> LSP.TextEdit
    makeTextEdit (range, textToReplace) =
      LSP.TextEdit
        { _range = SrcLoc.toLSPRange range,
          _newText = textToReplace
        }

sendCustomNotification :: (KnownSymbol s) => Proxy s -> JSON.Value -> ServerM ()
sendCustomNotification methodId json = LSP.sendNotification (LSP.SMethod_CustomMethod methodId) (json)

sendWindowShowMessage :: Text -> ServerM ()
sendWindowShowMessage message' = do
  let requestParams =
        LSP.ShowMessageRequestParams
          LSP.MessageType_Info
          message'
          Nothing
  _ <- LSP.sendRequest LSP.SMethod_WindowShowMessageRequest requestParams (\_ -> return ())
  return ()

sendWindowInfoMessage :: Text -> ServerM ()
sendWindowInfoMessage msg =
  LSP.sendNotification
    LSP.SMethod_WindowShowMessage
    (LSP.ShowMessageParams LSP.MessageType_Info msg)

sendSemanticTokensRefresh :: ServerM ()
sendSemanticTokensRefresh = do
  _ <- LSP.sendRequest LSP.SMethod_WorkspaceSemanticTokensRefresh Nothing (\_ -> return ())
  return ()

sendInlayHintRefresh :: ServerM ()
sendInlayHintRefresh = do
  _ <- LSP.sendRequest LSP.SMethod_WorkspaceInlayHintRefresh Nothing (\_ -> return ())
  return ()
