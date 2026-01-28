{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handler.SemanticTokens where

import qualified Data.Set as Set
import qualified Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types (Position (..), SemanticTokenAbsolute (..))
import qualified Language.LSP.Protocol.Types as LSP
import Server.Monad (FileState (..), ServerM, loadFileState, logText)
import Server.PositionMapping (PositionDelta (..), PositionResult (..))

handler :: LSP.Uri -> (Either (LSP.ResponseError) (LSP.SemanticTokens LSP.|? LSP.Null) -> ServerM ()) -> ServerM ()
handler fileUri responder = do
  logText "semantic token: start\n"
  case LSP.uriToFilePath fileUri of
    Nothing -> respondError (LSP.ResponseError (LSP.InR LSP.ErrorCodes_InvalidParams) "Invalid uri" Nothing)
    Just filePath -> do
      maybeFileState <- loadFileState filePath
      case maybeFileState of
        Nothing -> responder (Right $ LSP.InR LSP.Null) -- No file state yet, return null (let client try later)
        Just (FileState {semanticTokens = oldSemanticTokenAbsolutes, positionDelta}) ->
          do
            let newSemanticTokenAbsolutes = translatePositions positionDelta oldSemanticTokenAbsolutes
            let legend =
                  LSP.SemanticTokensLegend
                    (map LSP.toEnumBaseType $ Set.toList (LSP.knownValues @LSP.SemanticTokenTypes))
                    (map LSP.toEnumBaseType $ Set.toList (LSP.knownValues @LSP.SemanticTokenModifiers))
            case LSP.makeSemanticTokens legend newSemanticTokenAbsolutes of
              Left _errorMessage -> respondError (LSP.ResponseError (LSP.InR LSP.ErrorCodes_InternalError) _errorMessage Nothing)
              Right semanticTokens -> respondResult semanticTokens
  where
    respondResult :: LSP.SemanticTokens -> ServerM ()
    respondResult result = responder (Right $ LSP.InL result)
    respondError :: LSP.ResponseError -> ServerM ()
    respondError err = responder (Left err)

translatePosition :: PositionDelta -> LSP.SemanticTokenAbsolute -> Maybe LSP.SemanticTokenAbsolute
translatePosition PositionDelta {toDelta} oldToken@(LSP.SemanticTokenAbsolute {_line, _startChar}) =
  case toDelta oldPosition of
    PositionExact (Position {_line, _character}) -> Just (oldToken {_line = _line, _startChar = _character})
    _ -> Nothing
  where
    oldPosition :: LSP.Position
    oldPosition = LSP.Position _line _startChar

translatePositions :: PositionDelta -> [LSP.SemanticTokenAbsolute] -> [LSP.SemanticTokenAbsolute]
translatePositions positionDelta oldTokens = do
  oldToken <- oldTokens
  case translatePosition positionDelta oldToken of
    Nothing -> []
    Just newToken -> return newToken
