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
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import Data.IORef
  ( IORef,
    modifyIORef,
    newIORef,
    readIORef,
  )
import Data.Loc (posCol)
import Data.Loc.Range (Range, rangeStart)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Text
import qualified Data.Text as Text
import GCL.Predicate (PO, Spec (Specification, specID))
import GCL.WP.Types (StructWarning)
import GHC.TypeLits (KnownSymbol)
import qualified Language.LSP.Diagnostics as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as LSP
import Server.IntervalMap (IntervalMap)
import Server.PositionMapping (PositionDelta)
import qualified Server.SrcLoc as SrcLoc
import qualified Syntax.Abstract as Abstract
import qualified Syntax.Concrete as Concrete
import qualified Syntax.Typed as Typed

-- | State shared by all clients and requests
data GlobalState = GlobalState
  { logChannel :: Chan Text, -- Channel for printing log
    filesState :: IORef (Map FilePath FileState)
  }

type Versioned a = (LSP.Int32, a)

data FileState = FileState
  -- main states for Reload and Refine
  { refinedVersion :: LSP.Int32, -- the version number of the last refine
    specifications :: [Versioned Spec], -- editedVersion or (editedVersion + 1)
    proofObligations :: [Versioned PO], -- editedVersion
    warnings :: [Versioned StructWarning],
    didChangeShouldReload :: Int, -- trigger a reload after the server sends an edit
    -- SEE: increaseDidChangeShouldReload

    -- to support other LSP methods in a light-weighted manner
    loadedVersion :: LSP.Int32, -- the version number of the last reload
    toOffsetMap :: SrcLoc.ToOffset,
    concrete :: Concrete.Program,
    semanticTokens :: [LSP.SemanticTokenAbsolute],
    abstract :: Abstract.Program,
    idCount :: Int,
    definitionLinks :: IntervalMap LSP.LocationLink,
    hoverInfos :: IntervalMap LSP.Hover,
    elaborated :: Typed.Program,
    positionDelta :: PositionDelta, -- loadedVersion ~> editedVersion
    editedVersion :: LSP.Int32 -- the version number of the last change
  }
  deriving (Show)

-- | Constructs an initial global state
initGlobalEnv :: IO GlobalState
initGlobalEnv =
  GlobalState
    <$> newChan
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

loadFileState :: FilePath -> ServerM (Maybe FileState)
loadFileState filePath = do
  logText "ask file state ref\n"
  fileStateRef <- lift $ asks filesState
  logText "ask file state map\n"
  fileStateMap <- liftIO $ readIORef fileStateRef
  logText "lookup file state map\n"
  case Map.lookup filePath fileStateMap of
    Nothing -> do
      logText "  not found\n"
      return Nothing
    Just loadedFileState -> do
      logText "  found\n"
      return $ Just loadedFileState

saveFileState :: FilePath -> FileState -> ServerM ()
saveFileState filePath fileState = do
  logTextLn ">>>> saveFileState: fileState"
  logTextLn . Text.pack . show $ fileState
  logTextLn "<<<< saveFileState: fileState"
  fileStateRef <- lift $ asks filesState
  liftIO $ modifyIORef fileStateRef (Map.insert filePath fileState)

logFileState :: (Show a) => FilePath -> (FileState -> a) -> ServerM ()
logFileState filePath f = do
  logText "====== "
  logText . Text.pack $ filePath
  logText " ======\n"
  logText "\n"
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> logText "not loaded yet\n"
    Just fileState -> do
      logText "loaded\n"
      logText . Text.pack . Prelude.show $ f fileState
      logText "\n"
  logText "=======================\n"

modifyFileState :: FilePath -> (FileState -> FileState) -> ServerM ()
modifyFileState filePath modifier = do
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing ->
      logText "modifyFileState: not found\n"
    Just fileState -> do
      logText "modifyFileState: found\n"
      let fileState' = modifier fileState
      saveFileState filePath fileState'

bumpVersion :: FilePath -> ServerM ()
bumpVersion filePath = do
  modifyFileState filePath (\fileState@FileState {editedVersion} -> fileState {editedVersion = editedVersion + 1})

updateIdCounter ::
  FilePath ->
  Int ->
  ServerM ()
updateIdCounter filePath count' = do
  modifyFileState filePath (\fileState -> fileState {idCount = count'})

saveEditedVersion :: FilePath -> LSP.Int32 -> ServerM ()
saveEditedVersion filePath version = do
  modifyFileState filePath (\fileState -> fileState {editedVersion = version})

pushSpecs :: LSP.Int32 -> FilePath -> [Spec] -> ServerM ()
pushSpecs version filePath newSpecs = do
  let newVersionedSpecs :: [Versioned Spec] = Prelude.map (\spec -> (version, spec)) newSpecs
  modifyFileState
    filePath
    ( \fileState@FileState {specifications} ->
        fileState {specifications = specifications ++ newVersionedSpecs}
    )

pushPos :: LSP.Int32 -> FilePath -> [PO] -> ServerM ()
pushPos version filePath newPos = do
  let newVersionedPos :: [Versioned PO] = Prelude.map (\po -> (version, po)) newPos
  modifyFileState
    filePath
    ( \fileState@FileState {proofObligations} ->
        fileState {proofObligations = proofObligations ++ newVersionedPos}
    )

pushWarnings :: LSP.Int32 -> FilePath -> [StructWarning] -> ServerM ()
pushWarnings version filePath newWarnings = do
  let newVersionedWarnings :: [Versioned StructWarning] = Prelude.map (\warning -> (version, warning)) newWarnings
  modifyFileState
    filePath
    ( \fileState@FileState {warnings} ->
        fileState {warnings = warnings ++ newVersionedWarnings}
    )

deleteSpec :: FilePath -> Spec -> ServerM ()
deleteSpec filePath Specification {specID = targetSpecId} = do
  modifyFileState
    filePath
    ( \filesState@FileState {specifications} ->
        filesState {specifications = Prelude.filter (\(_, Specification {specID}) -> specID /= targetSpecId) specifications}
    )

-- Sometimes the server needs to edit the source (e.g., digHoles),
-- but the callback after the edit cannot read the latest source from the virtual file system.
-- The updated source only becomes available in the didChange event right after the callback finishes.
-- Therefore, we set a flag here so that didChange can determine whether to trigger load.
increaseDidChangeShouldReload :: FilePath -> ServerM ()
increaseDidChangeShouldReload filePath =
  modifyFileState filePath (\filesState@FileState {didChangeShouldReload} -> filesState {didChangeShouldReload = didChangeShouldReload + 1})

runIfDecreaseDidChangeShouldReload :: FilePath -> (FilePath -> ServerM ()) -> ServerM ()
runIfDecreaseDidChangeShouldReload filePath action = do
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Just fileState | didChangeShouldReload fileState > 0 -> do
      let orig = didChangeShouldReload fileState
      logTextLn $ "didChangeShouldReload: orig: " <> Text.pack (Prelude.show orig)
      modifyFileState filePath (\fileState' -> fileState' {didChangeShouldReload = orig - 1})
      action filePath
    _ -> return ()

readSource :: FilePath -> ServerM (Maybe Text)
readSource filepath = do
  maybeVirtualFile <- LSP.getVirtualFile $ LSP.toNormalizedUri $ LSP.filePathToUri filepath
  case maybeVirtualFile of
    Nothing -> return Nothing
    Just virtualFile -> do
      logTextLn $ "readSource: LSP.virtualFileVersion: " <> Text.pack (Prelude.show $ LSP.virtualFileVersion virtualFile)
      return (Just $ LSP.virtualFileText virtualFile)

modifyPositionDelta :: FilePath -> (PositionDelta -> PositionDelta) -> ServerM ()
modifyPositionDelta filePath modifier = do
  modifyFileState filePath (\fileState@FileState {positionDelta} -> fileState {positionDelta = modifier positionDelta})

editTexts :: FilePath -> [(Range, Text)] -> ServerM () -> ServerM ()
editTexts filepath rangeTextPairs onSuccess = do
  let requestParams :: LSP.ApplyWorkspaceEditParams =
        LSP.ApplyWorkspaceEditParams
          { _label = Just "Resolve Spec",
            _edit =
              LSP.WorkspaceEdit
                { _changes = Nothing,
                  _documentChanges = Just [LSP.InL textDocumentEdit],
                  _changeAnnotations = Nothing
                }
          }
  _requestId <- LSP.sendRequest LSP.SMethod_WorkspaceApplyEdit requestParams (\_ -> onSuccess)
  return ()
  where
    textDocumentEdit :: LSP.TextDocumentEdit
    textDocumentEdit =
      LSP.TextDocumentEdit
        { _textDocument = LSP.OptionalVersionedTextDocumentIdentifier (LSP.filePathToUri filepath) (LSP.InL 0),
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

-- send diagnostics
-- NOTE: existing diagnostics would be erased if `diagnostics` is empty
sendDiagnostics :: FilePath -> [LSP.Diagnostic] -> ServerM ()
sendDiagnostics filePath diagnostics = do
  maybeFileState <- loadFileState filePath
  let maybeVersion = fmap editedVersion maybeFileState
  LSP.publishDiagnostics
    100
    (LSP.toNormalizedUri (LSP.filePathToUri filePath))
    maybeVersion
    (LSP.partitionBySource diagnostics)

digHoles :: FilePath -> [Range] -> ServerM () -> ServerM ()
digHoles filePath ranges onFinish = do
  logTextLn $ "    < DigHoles " <> Text.pack (Prelude.show ranges)
  let indent range = Text.replicate (posCol (rangeStart range) - 1) " "
  let diggedText range = "[!\n" <> indent range <> "\n" <> indent range <> "!]"
  editTexts filePath (Prelude.map (\range -> (range, diggedText range)) ranges) onFinish

sendDebugMessage :: Text -> ServerM ()
sendDebugMessage message' = do
  let requestParams =
        LSP.ShowMessageRequestParams
          LSP.MessageType_Info
          message'
          Nothing
  _ <- LSP.sendRequest LSP.SMethod_WindowShowMessageRequest requestParams (\_ -> return ())
  return ()
