
module Server where

import           Control.Concurrent             ( forkIO
                                                , readChan
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad           hiding ( guard )
import qualified Data.Text.IO                  as Text
import           GHC.IO.IOMode                  ( IOMode(..) )
import           System.IO                      ( openFile, hSetEncoding, utf8, hFlush )
import           Language.LSP.Server
import qualified Language.LSP.Protocol.Types   as LSP
                                         hiding ( TextDocumentSyncClientCapabilities(..)
                                                , DidChangeNotebookDocumentParams(..)
                                                , NotebookDocumentSyncOptions(..)
                                                , NotebookDocumentSyncRegistrationOptions(..)
                                                )
import           Network.Simple.TCP             ( HostPreference(Host)
                                                , serve
                                                )
import           Network.Socket                 ( socketToHandle )
import           Server.Handler                 ( handlers )
import Server.Monad (initGlobalEnv, runServerM, logChannel, GlobalState)
import qualified Data.Text as Text

--------------------------------------------------------------------------------

runOnPort :: String -> IO Int
runOnPort port = do
  env <- initGlobalEnv
  _threadId <- forkIO (printLog env)
  serve (Host "127.0.0.1") port $ \(sock, _remoteAddr) -> do
    putStrLn $ "== connection established at " ++ port ++ " =="

    -- WARN: these two parameters are need in the newer version
    -- but idk what to provide and `runOnPort` is not used so ¯\_(ツ)_/¯
    let ioLogger = undefined
    let lspLogger = undefined

    handle <- socketToHandle sock ReadWriteMode
    _      <- runServerWithHandles ioLogger lspLogger handle handle (serverDefn env)
    putStrLn "== dev server closed =="
 where
  printLog :: GlobalState -> IO ()
  printLog env = forever $ do
    result <- readChan (logChannel env)
    Text.putStrLn result

-- entry point of the LSP server
runOnStdio :: Maybe FilePath -> IO Int
runOnStdio maybeLogFile = do
  env <- initGlobalEnv
  case maybeLogFile of
    Nothing -> return ()
    Just logFile -> do
      writeFile logFile "=== Log file Start ===\n"
      writeFile logFile logFile
      _threadId <- forkIO (writeLog env logFile)
      return ()
  runServer (serverDefn env)
 where
  writeLog :: GlobalState -> FilePath -> IO ()
  writeLog env logFile = forever $ do
    handle <- openFile logFile AppendMode
    hSetEncoding handle utf8
    forever $ do
      result <- readChan (logChannel env)
      Text.hPutStr handle result
      hFlush handle

serverDefn :: GlobalState -> ServerDefinition ()
serverDefn env = ServerDefinition
  { defaultConfig         = ()
  , configSection         = Text.pack "" -- FIXME: idk what the put here
  , parseConfig           = const $ pure $ Right ()
  , onConfigChange        = const $ pure ()
  , doInitialize          = \ctxEnv _req -> pure $ Right ctxEnv
  , staticHandlers        = \_caps -> handlers
  , interpretHandler      = \ctxEnv -> Iso (runServerM env ctxEnv) liftIO
  , options               = lspOptions
  }

lspOptions :: Options
lspOptions = defaultOptions { optTextDocumentSync            = Just syncOptions
                            , optCompletionTriggerCharacters = Just ['\\']
                            }

-- these `TextDocumentSyncOptions` are essential for receiving notifications from the client
syncOptions :: LSP.TextDocumentSyncOptions
syncOptions = LSP.TextDocumentSyncOptions
  { LSP._openClose         = Just True -- receive open and close notifications from the client
  , LSP._change            = Just LSP.TextDocumentSyncKind_Incremental -- receive change notifications from the client
  , LSP._willSave          = Just False -- receive willSave notifications from the client
  , LSP._willSaveWaitUntil = Just False -- receive willSave notifications from the client
  , LSP._save              = Just $ LSP.InR saveOptions
  }

-- includes the document content on save, so that we don't have to read it from the disk
saveOptions :: LSP.SaveOptions
saveOptions = LSP.SaveOptions (Just True)
