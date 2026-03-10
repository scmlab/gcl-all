{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server.Monad where

import Control.Concurrent
  ( Chan,
    newChan,
    threadDelay,
    writeChan,
  )
import Control.Exception (SomeException, catch, displayException, throwIO)
import Control.Monad (foldM)
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
import Data.Maybe (mapMaybe)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Predicate (Hole (..), Origin (..), PO (..), Spec (..))
import GCL.Range (MaybeRanged (..), Range, posCol, rangeStart)
import GCL.WP.Types (StructWarning (..))
import GHC.TypeLits (KnownSymbol)
import qualified Language.LSP.Diagnostics as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as LSP
import Server.Change
  ( GCLMove,
    LSPMove,
    applyGCLMove,
    applyGCLMoveToContainerRange,
    applyLSPMovesToToken,
    applyMovesToIntervalMap,
    fromLSPMove,
    updateOriginTargetRanges,
  )
import Server.GoToDefn (OriginTargetRanges)
import Server.IntervalMap (IntervalMap)
import qualified Server.SrcLoc as SrcLoc

-- | State shared by all clients and requests
data GlobalState = GlobalState
  { logChannel :: Chan Text, -- Channel for printing log
    filesState :: IORef (Map FilePath FileState),
    pendingEdits :: IORef (Map FilePath PendingEdit)
  }

data FileState = FileState
  { fsSpecifications :: ![Spec],
    fsHoles :: ![Hole],
    fsProofObligations :: ![PO],
    fsWarnings :: ![StructWarning],
    fsIdCount :: !Int,
    fsSemanticTokens :: ![LSP.SemanticTokenAbsolute],
    fsDefinitionLinks :: !(IntervalMap OriginTargetRanges),
    fsHoverInfos :: !(IntervalMap LSP.Hover)
  }

data PendingEdit = PendingEdit
  { expectedContent :: !Text,
    pendingFileState :: !FileState
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

getPendingEdit :: FilePath -> ServerM (Maybe PendingEdit)
getPendingEdit filePath = do
  ref <- lift $ asks pendingEdits
  m <- liftIO $ readIORef ref
  return $ Map.lookup filePath m

setPendingEdit :: FilePath -> PendingEdit -> ServerM ()
setPendingEdit filePath pe = do
  ref <- lift $ asks pendingEdits
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
editTextsWithVersion :: FilePath -> LSP.Int32 -> [(Range, Text)] -> ServerM ()
editTextsWithVersion filepath version rangeTextPairs = do
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

data HoleKind
  = StmtHole
  | ExprHole
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- FileState translation

translateFileState :: [LSPMove] -> FileState -> FileState
translateFileState lspMoves fs =
  let gclMoves = map fromLSPMove lspMoves
   in fs
        { fsSpecifications = mapMaybe (translateSpecRange gclMoves) (fsSpecifications fs),
          fsHoles = mapMaybe (translateHoleRange gclMoves) (fsHoles fs),
          fsProofObligations = mapMaybe (translatePoRange gclMoves) (fsProofObligations fs),
          fsWarnings = mapMaybe (translateWarningRange gclMoves) (fsWarnings fs),
          fsSemanticTokens = mapMaybe (applyLSPMovesToToken lspMoves) (fsSemanticTokens fs),
          fsDefinitionLinks = applyMovesToIntervalMap gclMoves updateOriginTargetRanges (fsDefinitionLinks fs),
          fsHoverInfos = applyMovesToIntervalMap gclMoves (\_ h -> Just h) (fsHoverInfos fs)
        }

-- 目前只維護 specRange，而沒有更新 specPre 和 specPost 裡面的位置資訊
translateSpecRange :: [GCLMove] -> Spec -> Maybe Spec
translateSpecRange moves spec@Specification {specRange = oldRange} = do
  newRange <- foldM applyGCLMoveToContainerRange oldRange moves
  return $ spec {specRange = newRange}

-- 目前只維護 poOrigin 裡面的 location，而沒有更新 poPre 和 poPost 裡面的位置資訊
translatePoRange :: [GCLMove] -> PO -> Maybe PO
translatePoRange moves po@PO {poOrigin} = do
  oldRange <- maybeRangeOf poOrigin
  newRange <- foldM applyGCLMoveToContainerRange oldRange moves
  return $ po {poOrigin = setOriginRange (Just newRange) poOrigin}

-- 目前只維護 holeRange，而沒有更新 holeType 裡面的位置資訊
translateHoleRange :: [GCLMove] -> Hole -> Maybe Hole
translateHoleRange moves hole@Hole {holeRange = oldRange} = do
  newRange <- foldM applyGCLMoveToContainerRange oldRange moves
  return $ hole {holeRange = newRange}

translateWarningRange :: [GCLMove] -> StructWarning -> Maybe StructWarning
translateWarningRange moves (MissingBound oldRange) = do
  newRange <- foldM applyGCLMoveToContainerRange oldRange moves
  return $ MissingBound newRange

setOriginRange :: Maybe Range -> Origin -> Origin
setOriginRange l (AtAbort _) = AtAbort l
setOriginRange l (AtSkip _) = AtSkip l
setOriginRange l (AtSpec _) = AtSpec l
setOriginRange l (AtAssignment _) = AtAssignment l
setOriginRange l (AtAssertion _) = AtAssertion l
setOriginRange l (AtIf _) = AtIf l
setOriginRange l (AtLoop _) = AtLoop l
setOriginRange l (AtTermination _) = AtTermination l
setOriginRange l (Explain h e i p _) = Explain h e i p l

--------------------------------------------------------------------------------

sendDebugMessage :: Text -> ServerM ()
sendDebugMessage message' = do
  let requestParams =
        LSP.ShowMessageRequestParams
          LSP.MessageType_Info
          message'
          Nothing
  _ <- LSP.sendRequest LSP.SMethod_WindowShowMessageRequest requestParams (\_ -> return ())
  return ()
