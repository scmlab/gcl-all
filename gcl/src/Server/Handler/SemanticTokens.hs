{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handler.SemanticTokens where

import qualified Data.Set as Set
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Server.Monad (FileState3 (..), ServerM, getFileState3, logText)

handler :: LSP.Uri -> (Either (LSP.ResponseError) (LSP.SemanticTokens LSP.|? LSP.Null) -> ServerM ()) -> ServerM ()
handler fileUri responder = do
  logText "semantic token: start\n"
  case LSP.uriToFilePath fileUri of
    Nothing -> respondError (LSP.ResponseError (LSP.InR LSP.ErrorCodes_InvalidParams) "Invalid uri" Nothing)
    Just filePath -> do
      maybeFileState <- getFileState3 filePath
      case maybeFileState of
        Nothing -> respondError (LSP.ResponseError (LSP.InR LSP.ErrorCodes_ServerNotInitialized) "Please reload before requesting for semantic tokens." Nothing)
        Just (FileState3 {fs3SemanticTokens}) ->
          do
            let legend =
                  LSP.SemanticTokensLegend
                    (map LSP.toEnumBaseType $ Set.toList (LSP.knownValues @LSP.SemanticTokenTypes))
                    (map LSP.toEnumBaseType $ Set.toList (LSP.knownValues @LSP.SemanticTokenModifiers))
            case LSP.makeSemanticTokens legend fs3SemanticTokens of
              Left _errorMessage -> respondError (LSP.ResponseError (LSP.InR LSP.ErrorCodes_InternalError) _errorMessage Nothing)
              Right result -> respondResult result
  where
    respondResult :: LSP.SemanticTokens -> ServerM ()
    respondResult result = responder (Right $ LSP.InL result)
    respondError :: LSP.ResponseError -> ServerM ()
    respondError err = responder (Left err)
