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
import qualified Server.Handler.GCL.Refine as Refine
import qualified Server.Handler.GCL.Reload as Reload
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
        -- FIXME: go to definition doesn't work here?
        -- original code, I think it returns an empty list regardless
        -- GoToDefinition.handler uri position (responder . Right . LSP.InR . LSP.InR . LSP.List)
        GoToDefinition.handler uri position (responder . Right . LSP.InR . LSP.InR . (const LSP.Null))
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
      requestHandler (LSP.SMethod_CustomMethod (Proxy @"gcl/reload")) $ jsonMiddleware Reload.handler,
      -- "gcl/refine" - refine
      requestHandler (LSP.SMethod_CustomMethod (Proxy @"gcl/refine")) $ jsonMiddleware Refine.handler,
      -- "gcl/debug" - debug FileState
      requestHandler (LSP.SMethod_CustomMethod (Proxy @"gcl/debug")) $ jsonMiddleware Debug.handler
    ]

-- | A handler type for custom LSP methods.
--   Takes parsed request params, a success callback, and an error callback.
--   Calls one of the callbacks to return a result or error to the client.
type CustomMethodHandler params result err = params -> (result -> ServerM ()) -> (err -> ServerM ()) -> ServerM ()

{-# ANN jsonMiddleware ("HLint: ignore Redundant lambda" :: String) #-}
-- converts the request JSON object into specific request params (as a Haskell record) for the handler
-- TODO: maybe we don't need an error callback
jsonMiddleware ::
  (KnownSymbol s, JSON.FromJSON params, JSON.ToJSON result, JSON.ToJSON err) =>
  CustomMethodHandler params result err ->
  -- -> LSP.Handler ServerM (LSP.CustomMethod :: LSP.Method LSP.FromClient LSP.Request)
  LSP.Handler ServerM (LSP.Method_CustomMethod s :: LSP.Method LSP.ClientToServer LSP.Request)
jsonMiddleware handler = \req responder -> do
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
      handler
        params
        (responder . Right . JSON.toJSON)
        (responder . Left . Hack.resToTRes . makeInternalError)

decodeMessageParams :: forall a. (JSON.FromJSON a) => JSON.Value -> Either LSP.ResponseError a
decodeMessageParams json = do
  case JSON.fromJSON json :: JSON.Result a of
    JSON.Success params -> Right params
    JSON.Error msg -> Left (makeParseError ("Json decoding failed." <> Text.pack msg))

makeInternalError :: (JSON.ToJSON e) => e -> LSP.ResponseError
makeInternalError err =
  LSP.ResponseError
    { _code = LSP.InR LSP.ErrorCodes_InternalError,
      _message = "",
      _xdata = Just (JSON.toJSON err)
    }

makeParseError :: Text -> LSP.ResponseError
makeParseError message =
  LSP.ResponseError
    { _code = LSP.InR LSP.ErrorCodes_ParseError,
      _message = message,
      _xdata = Nothing
    }
