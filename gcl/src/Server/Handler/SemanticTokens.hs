{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Server.Handler.SemanticTokens where

import qualified Data.Set as Set
import Server.Monad (ServerM, FileState (..), loadFileState, logText)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types (SemanticTokenAbsolute(..), Position(..))
import Server.PositionMapping (PositionDelta(..), PositionResult(..))

handler :: LSP.Uri -> (Either LSP.ResponseError (Maybe LSP.SemanticTokens) -> ServerM ()) -> ServerM ()
handler fileUri responder = do
  logText "semantic token: start\n"
  case LSP.uriToFilePath fileUri of
    Nothing       -> respondError (LSP.ResponseError (LSP.InR LSP.ErrorCodes_InvalidParams) "Invalid uri" Nothing)
    Just filePath -> do
      maybeFileState <- loadFileState filePath
      case maybeFileState of
        Nothing                          -> respondError (LSP.ResponseError (LSP.InR LSP.ErrorCodes_ServerNotInitialized) "Please reload before requesting for semantic tokens." Nothing)
        Just (FileState{semanticTokens = oldSemanticTokenAbsolutes, positionDelta})
                                         -> do
          let newSemanticTokenAbsolutes = translatePositions positionDelta oldSemanticTokenAbsolutes
          let legend = LSP.SemanticTokensLegend
                        (Set.toList $ Set.map LSP.toEnumBaseType (LSP.knownValues @LSP.SemanticTokenTypes))
                        (Set.toList $ Set.map LSP.toEnumBaseType (LSP.knownValues @LSP.SemanticTokenModifiers))
          case LSP.makeSemanticTokens legend newSemanticTokenAbsolutes of
            Left _errorMessage   -> respondError (LSP.ResponseError (LSP.InR LSP.ErrorCodes_InternalError) _errorMessage Nothing)
            Right semanticTokens -> respondResult semanticTokens
  where
    respondResult :: LSP.SemanticTokens -> ServerM ()
    respondResult result = responder (Right $ Just result)
    respondError :: LSP.ResponseError -> ServerM ()
    respondError err = responder (Left err)

translatePosition :: PositionDelta -> LSP.SemanticTokenAbsolute -> Maybe LSP.SemanticTokenAbsolute
translatePosition PositionDelta{toDelta} oldToken@(LSP.SemanticTokenAbsolute{_line, _startChar}) =
  case toDelta oldPosition of
    PositionExact (Position{_line, _character}) -> Just (oldToken{_line = _line, _startChar = _character})
    _                                           -> Nothing
  where
    oldPosition :: LSP.Position
    oldPosition = LSP.Position _line _startChar

translatePositions :: PositionDelta -> [LSP.SemanticTokenAbsolute] -> [LSP.SemanticTokenAbsolute]
translatePositions positionDelta oldTokens = do
  oldToken <- oldTokens
  case translatePosition positionDelta oldToken of
    Nothing       -> []
    Just newToken -> return newToken
