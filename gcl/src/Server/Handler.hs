{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server.Handler (handlers) where

import Control.Lens ((^.))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSONText
import Data.Bifunctor (first)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import GHC.TypeLits (KnownSymbol)
import qualified Hack
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
  ( Handlers,
    notificationHandler,
    requestHandler,
  )
import qualified Language.LSP.Server as LSP
import qualified Server.Handler.AutoCompletion as AutoCompletion
import qualified Server.Handler.GCL.Debug as Debug
import qualified Server.Handler.GCL.Load as Load
import qualified Server.Handler.GCL.Refine as Refine
import qualified Server.Handler.GoToDefinition as GoToDefinition
import qualified Server.Handler.Hover as Hover
import qualified Server.Handler.Initialized as Initialized
import qualified Server.Handler.OnDidChangeTextDocument as OnDidChangeTextDocument
import qualified Server.Handler.SemanticTokens as SemanticTokens
import Server.Load (load)
import Server.Monad (ServerM, logText)

-- handlers of the LSP server
handlers :: Handlers ServerM
handlers =
  mconcat
    [ -- "initialized" - after initialize
      notificationHandler LSP.SMethod_Initialized $ \_ntf -> do
        logText "SMethod_Initialized is called.\n"
        Initialized.handler,
      -- "textDocument/didOpen" - after open
      notificationHandler LSP.SMethod_TextDocumentDidOpen $ \ntf -> do
        logText "SMethod_TextDocumentDidOpen start\n"
        let uri = ntf ^. (LSP.params . LSP.textDocument . LSP.uri)
        case LSP.uriToFilePath uri of
          Nothing -> return ()
          Just filePath -> load filePath
        logText "SMethod_TextDocumentDidOpen end\n",
      -- "textDocument/didChange" - after every edition
      notificationHandler LSP.SMethod_TextDocumentDidChange $ \ntf -> do
        logText "SMethod_TextDocumentDidChange start\n"
        let uri = ntf ^. (LSP.params . LSP.textDocument . LSP.uri)
        let changes = ntf ^. (LSP.params . LSP.contentChanges)
        case LSP.uriToFilePath uri of
          Nothing -> return ()
          Just filePath -> OnDidChangeTextDocument.handler filePath changes
        logText "SMethod_TextDocumentDidChange end\n",
      -- "textDocument/didSave" - after save
      notificationHandler LSP.SMethod_TextDocumentDidSave $ \_ntf -> do
        logText "SMethod_TextDocumentDidSave start\n"
        logText "SMethod_TextDocumentDidSave end\n",
      -- "textDocument/didClose" - after close
      notificationHandler LSP.SMethod_TextDocumentDidClose $ \_ntf -> do
        logText "SMethod_TextDocumentDidClose start\n"

        -- TODO: this handler is only a stub because VSCode complains
        -- maybe add something here in the future

        logText "SMethod_TextDocumentDidClose end\n",
      -- "workspace/didChangeConfiguration"
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_ntf -> do
        logText "SMethod_WorkspaceDidChangeConfiguration start\n"

        -- TODO: this handler is only a stub because VSCode complains
        -- maybe add something here in the future

        logText "SMethod_WorkspaceDidChangeConfiguration end\n",
      -- "textDocument/completion" - auto-completion
      requestHandler LSP.SMethod_TextDocumentCompletion $ \req responder -> do
        let completionContext = req ^. LSP.params . LSP.context
        let position = req ^. LSP.params . LSP.position
        l <- AutoCompletion.handler position completionContext
        (responder . Right . LSP.InR . LSP.InL) l,
      -- "textDocument/definition" - go to definition
      requestHandler LSP.SMethod_TextDocumentDefinition $ \req responder -> do
        logText "SMethod_TextDocumentDefinition is called.\n"
        let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
        let position = req ^. (LSP.params . LSP.position)
        -- responder takes an
        -- Either (TResponseError 'Method_TextDocumentDefinition) (Definition |? ([DefinitionLink] |? Null))
        --                                                  Right ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        --                                                                   InR ^^^^^^^^^^^^^^^^^^^^^^^^^^
        --                                                                    InL ^^^^^^^^^^^^^^^^
        GoToDefinition.handler uri position (responder . Right . LSP.InR . LSP.InL . fmap LSP.DefinitionLink)
        logText "SMethod_TextDocumentDefinition is finished.\n",
      -- "textDocument/hover" - get hover information
      requestHandler LSP.SMethod_TextDocumentHover $ \req responder -> do
        let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
        let pos = req ^. (LSP.params . LSP.position)
        Hover.handler uri pos (responder . Right),
      -- "textDocument/semanticTokens/full" - get all semantic tokens
      requestHandler LSP.SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
        let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
        SemanticTokens.handler uri (responder . first Hack.resToTRes),
      -- "gcl/reload" - reload
      requestHandler (LSP.SMethod_CustomMethod (Proxy @"gcl/reload")) $ customRequestMiddleware Load.handler,
      -- "gcl/refine" - refine
      requestHandler (LSP.SMethod_CustomMethod (Proxy @"gcl/refine")) $ customRequestMiddleware Refine.handler,
      -- "gcl/debug" - debug FileState
      requestHandler (LSP.SMethod_CustomMethod (Proxy @"gcl/debug")) $ customRequestMiddleware Debug.handler
    ]

-- Extracts the common boilerplate shared by custom LSP request handlers.
-- Each custom method provides its own (params -> ServerM result).
-- Application-level errors are sent to the client directly by the handler itself,
-- rather than through the return value. NOTE: could be refactored to return Either.
{-# ANN customRequestMiddleware ("HLint: ignore Redundant lambda" :: String) #-}
customRequestMiddleware ::
  (KnownSymbol s, JSON.FromJSON params, JSON.ToJSON result) =>
  -- wraps a custom handler
  (params -> ServerM result) ->
  -- to a LSP Request Handler (SEE: type family Handler)
  ( LSP.TRequestMessage (LSP.Method_CustomMethod s) -> -- req
    (Either (LSP.TResponseError (LSP.Method_CustomMethod s)) JSON.Value -> ServerM ()) -> -- responder
    ServerM ()
  )
customRequestMiddleware customHandler = \req responder -> do
  logText "json: decoding request\n"
  let json = req ^. LSP.params
  logText $ "JSON content: " <> TextLazy.toStrict (JSONText.encodeToLazyText json) <> "\n"
  case decodeMessageParams json of
    Left err -> do
      logText "json: decoding failed with\n"
      logText (Text.pack . show $ JSON.encode json)
      responder (Left $ Hack.resToTRes err)
    Right params -> do
      logText "json: decoding succeeded\n"
      result <- customHandler params
      responder (Right $ JSON.toJSON result)

decodeMessageParams :: forall a. (JSON.FromJSON a) => JSON.Value -> Either LSP.ResponseError a
decodeMessageParams json = do
  case JSON.fromJSON json :: JSON.Result a of
    JSON.Success params -> Right params
    JSON.Error msg -> Left (makeParseError ("Json decoding failed." <> Text.pack msg))

makeParseError :: Text -> LSP.ResponseError
makeParseError message =
  LSP.ResponseError
    { _code = LSP.InR LSP.ErrorCodes_ParseError,
      _message = message,
      _xdata = Nothing
    }
