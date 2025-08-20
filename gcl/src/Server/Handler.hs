{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server.Handler ( handlers ) where

import           GHC.TypeLits                   ( KnownSymbol )
import           Data.Bifunctor                 ( first )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Control.Monad                  ( when )
import           Control.Lens                   ( (^.) )
import qualified Data.Aeson                     as JSON
import Data.Text (Text)
import qualified Data.Aeson.Text                as JSONText
import qualified Data.Text.Lazy                 as TextLazy
import           Language.LSP.Server            ( Handlers
                                                , notificationHandler
                                                , requestHandler
                                                )

import qualified Language.LSP.Protocol.Types    as LSP
import qualified Language.LSP.Protocol.Lens     as LSP
import qualified Language.LSP.Protocol.Message  as LSP
import qualified Language.LSP.Server            as LSP

import qualified Server.Handler.Initialized    as Initialized
import qualified Server.Handler.GoToDefinition as GoToDefinition
import qualified Server.Handler.AutoCompletion as AutoCompletion
import qualified Server.Handler.Hover          as Hover
import qualified Server.Handler.SemanticTokens as SemanticTokens
import qualified Server.Handler.GCL.Reload     as Reload
import qualified Server.Handler.GCL.Refine     as Refine
import Server.Monad (ServerM, logText)
import Server.Load (load)
import qualified Server.Handler.OnDidChangeTextDocument as OnDidChangeTextDocument
import qualified Data.Text as Text

import Hack
import Language.LSP.Protocol.Message (TResponseError(TResponseError))

-- handlers of the LSP server
handlers :: Handlers ServerM
handlers = mconcat
  [ -- "initialized" - after initialize
    notificationHandler LSP.SMethod_Initialized $ \_ntf -> do
      logText "SInitialized is called.\n"
      Initialized.handler
  , -- "textDocument/didOpen" - after open
    notificationHandler LSP.SMethod_TextDocumentDidOpen $ \ntf -> do
      logText "STextDocumentDidOpen start\n"
      let uri            = ntf ^. (LSP.params . LSP.textDocument . LSP.uri)
      case LSP.uriToFilePath uri of
        Nothing       -> return ()
        Just filePath -> load filePath
      logText "STextDocumentDidOpen end\n"
  , -- "textDocument/didChange" - after every edition
    notificationHandler LSP.SMethod_TextDocumentDidChange $ \ntf -> do
      logText "STextDocumentDidChange start\n"
      let uri :: LSP.Uri = ntf ^. (LSP.params . LSP.textDocument . LSP.uri)
      let changes        = ntf ^. (LSP.params . LSP.contentChanges)
      case LSP.uriToFilePath uri of
        Nothing       -> return ()
        Just filePath -> OnDidChangeTextDocument.handler filePath changes
      logText "STextDocumentDidChange end\n"
  , -- "textDocument/completion" - auto-completion
    requestHandler LSP.SMethod_TextDocumentCompletion $ \req responder -> do
      let completionContext = req ^. LSP.params . LSP.context
      let position          = req ^. LSP.params . LSP.position
      l <- AutoCompletion.handler position completionContext
      (responder . Right . LSP.InR . LSP.InL) l
  , -- "textDocument/definition" - go to definition
    requestHandler LSP.SMethod_TextDocumentDefinition $ \req responder -> do
      logText "STextDocumentDefinition is called.\n"
      let uri      = req ^. (LSP.params . LSP.textDocument . LSP.uri)
      let position = req ^. (LSP.params . LSP.position)
      -- FIXME: go to definition doesn't work here?
      -- original code, I think it returns an empty list regardless
      -- GoToDefinition.handler uri position (responder . Right . LSP.InR . LSP.InR . LSP.List)
      GoToDefinition.handler uri position (responder . Right . LSP.InR . LSP.InR . (const LSP.Null))
      logText "STextDocumentDefinition is finished.\n"
  , -- "textDocument/hover" - get hover information
    requestHandler LSP.SMethod_TextDocumentHover $ \req responder -> do
      let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
      let pos = req ^. (LSP.params . LSP.position)
      Hover.handler uri pos (responder . Right)
  , -- "textDocument/semanticTokens/full" - get all semantic tokens
    requestHandler LSP.SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
      let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
      -- HACK: this is probably incorrect
      -- see also:
      -- - https://hackage.haskell.org/package/lsp-types-2.3.0.1/docs/Language-LSP-Protocol-Message.html#t:ResponseError
      -- - https://hackage.haskell.org/package/lsp-types-2.3.0.1/docs/Language-LSP-Protocol-Message.html#t:TResponseError
      -- - https://hackage.haskell.org/package/lsp-types-2.3.0.1/docs/Language-LSP-Protocol-Message.html#t:ErrorData
      let resToTRes (LSP.ResponseError c m _) = LSP.TResponseError c m Nothing
      SemanticTokens.handler uri (responder . first resToTRes)
  , -- "gcl/reload" - reload
    requestHandler (LSP.SMethod_CustomMethod (Proxy @"gcl/reload")) $ jsonMiddleware Reload.handler
  , -- "gcl/refine" - refine
    requestHandler (LSP.SMethod_CustomMethod (Proxy @"gcl/refine")) $ jsonMiddleware Refine.handler
  ]

-- | A handler type for custom LSP methods.
--   Takes parsed request params, a success callback, and an error callback.
--   Calls one of the callbacks to return a result or error to the client.
type CustomMethodHandler params result err = params -> (result -> ServerM ()) -> (err -> ServerM ()) -> ServerM ()


{-# ANN jsonMiddleware ("HLint: ignore Redundant lambda" :: String) #-}
-- converts the request JSON object into specific request params (as a Haskell record) for the handler
-- TODO: maybe we don't need an error callback
jsonMiddleware :: (KnownSymbol s, JSON.FromJSON params, JSON.ToJSON result, JSON.ToJSON err)
                  => CustomMethodHandler params result err
                  -- -> LSP.Handler ServerM (LSP.CustomMethod :: LSP.Method LSP.FromClient LSP.Request)
                  -> LSP.Handler ServerM (LSP.Method_CustomMethod s :: LSP.Method LSP.ClientToServer LSP.Request)
jsonMiddleware handler = \req responder -> do
  logText "json: decoding request\n"
  let json = req ^. LSP.params
  logText $ "JSON content: " <> TextLazy.toStrict (JSONText.encodeToLazyText json) <> "\n"
  case decodeMessageParams json of
    Left err -> do
      logText "json: decoding failed with\n"
      logText (Text.pack . show $ JSON.encode json)
      responder (Left err)
    Right params -> do
      logText "json: decoding succeeded\n"
      handler params
        (responder . Right . JSON.toJSON)
        (responder. Left . makeInternalError)

decodeMessageParams :: forall a. JSON.FromJSON a => JSON.Value -> Either LSP.ResponseError a
decodeMessageParams json = do
  case JSON.fromJSON json :: JSON.Result a of
    JSON.Success params -> Right params
    JSON.Error msg            -> Left (makeParseError ("Json decoding failed." <> Text.pack msg))

makeInternalError :: JSON.ToJSON e => e -> LSP.ResponseError
makeInternalError err = LSP.ResponseError 
    { _code    = LSP.InR LSP.ErrorCodes_InternalError
    , _message = ""
    , _xdata   = Just (JSON.toJSON err)
    }

makeParseError :: Text -> LSP.ResponseError
makeParseError message = LSP.ResponseError 
    { _code    = LSP.InR LSP.ErrorCodes_ParseError
    , _message = message
    , _xdata   = Nothing
    }
