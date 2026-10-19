module Server where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import qualified Language.LSP.Protocol.Types as LSP hiding
  ( DidChangeNotebookDocumentParams (..),
    NotebookDocumentSyncOptions (..),
    NotebookDocumentSyncRegistrationOptions (..),
    TextDocumentSyncClientCapabilities (..),
  )
import Language.LSP.Server
import Server.Handler (handlers)
import Server.Monad (GlobalState, initGlobalEnv, runServerMLogError)

--------------------------------------------------------------------------------

-- entry point of the LSP server
runOnStdio :: IO Int
runOnStdio = do
  env <- initGlobalEnv
  runServer (serverDefn env)

serverDefn :: GlobalState -> ServerDefinition ()
serverDefn env =
  ServerDefinition
    { defaultConfig = (),
      configSection = Text.pack "", -- FIXME: idk what the put here
      parseConfig = const $ pure $ Right (),
      onConfigChange = const $ pure (),
      doInitialize = \ctxEnv _req -> pure $ Right ctxEnv,
      staticHandlers = \_caps -> handlers,
      interpretHandler = \ctxEnv -> Iso (runServerMLogError env ctxEnv) liftIO,
      options = lspOptions
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions,
      optCompletionTriggerCharacters = Just ['\\']
    }

-- these `TextDocumentSyncOptions` are essential for receiving notifications from the client
syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just True, -- receive open and close notifications from the client
      LSP._change = Just LSP.TextDocumentSyncKind_Incremental, -- receive change notifications from the client
      LSP._willSave = Just False, -- receive willSave notifications from the client
      LSP._willSaveWaitUntil = Just False, -- receive willSave notifications from the client
      LSP._save = Just $ LSP.InR saveOptions
    }

-- includes the document content on save, so that we don't have to read it from the disk
saveOptions :: LSP.SaveOptions
saveOptions = LSP.SaveOptions (Just True)
