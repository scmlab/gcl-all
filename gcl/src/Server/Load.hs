{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use join" #-}

module Server.Load (LoadResponse (..), load) where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Range (Range, posCol, rangeStart)
import qualified GCL.Type as TypeChecking
import qualified GCL.WP as WP
import GHC.Generics (Generic)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as VFS
import Server.GoToDefn (collectLocationLinks)
import Server.Highlighting (collectHighlighting)
import Server.Hover (collectHoverInfo)
import Server.Monad (FileState (..), ServerM, loadFileState, logText, saveFileState)
import Server.Notification.Error (sendErrorNotification)
import Server.Notification.Update (sendUpdateNotification)
import Server.PositionMapping (idDelta)
import qualified Server.SrcLoc as SrcLoc
import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import Syntax.Concrete.Types (GdCmd (..), SepBy (..))
import qualified Syntax.Parser as Parser
import qualified Syntax.Typed as T

-- | Response type for load operation
data LoadResponse
  = LoadDone
  | LoadNeedsEdit {textDocumentEdit :: LSP.TextDocumentEdit}
  deriving (Eq, Show, Generic)

instance JSON.ToJSON LoadResponse where
  toJSON LoadDone = JSON.object [("status", JSON.String "done")]
  toJSON (LoadNeedsEdit textDocEdit) =
    JSON.object
      [ ("status", JSON.String "needsEdit"),
        ("textDocumentEdit", JSON.toJSON textDocEdit)
      ]

-- | Generate the replacement text for a hole (same logic as digHoles)
diggedText :: Range -> Text
diggedText range =
  let indent = Text.replicate (posCol (rangeStart range) - 1) " "
   in "[!\n" <> indent <> "\n" <> indent <> "!]"

-- | Convert GCL Range to LSP TextEdit
rangeToTextEdit :: Range -> Text -> LSP.TextEdit
rangeToTextEdit range newText =
  LSP.TextEdit
    { _range = SrcLoc.toLSPRange range,
      _newText = newText
    }

load :: FilePath -> ServerM LoadResponse
load filePath = do
  logText "load: start\n"
  maybeFileState <- loadFileState filePath
  let currentVersion = case maybeFileState of
        Nothing -> 0
        Just FileState {editedVersion} -> editedVersion

  -- Get virtual file version for TextDocumentEdit
  maybeVirtualFile <- LSP.getVirtualFile $ LSP.toNormalizedUri $ LSP.filePathToUri filePath
  let vfsVersion = maybe 0 VFS.virtualFileVersion maybeVirtualFile

  -- read source
  let maybeSource = fmap VFS.virtualFileText maybeVirtualFile
  case maybeSource of
    Nothing -> do
      logText "  read error"
      onError (CannotReadFile filePath)
      logText "load: end\n"
      return LoadDone -- Return done even on error (error is sent via notification)
    Just source -> do
      result <- runExceptT $ do
        lift $ logText "  source read \n"
        -- parse source into concrete syntax
        concrete <- ExceptT $ parse filePath source
        -- check for holes
        case collectHoles concrete of
          [] -> do
            lift $ logText "  all holes digged\n"
            let abstract = C.toAbstract concrete
            lift $ logText "  abstract program generated\n"
            elaborated <- ExceptT $ elaborate abstract
            ExceptT $ case WP.sweep elaborated of
              Left err -> do
                logText "  sweep error\n"
                onError (StructError err)
                return $ Left LoadDone
              Right (pos, specs, warnings, _redexes, idCount) -> do
                let fileState =
                      FileState
                        { refinedVersion = currentVersion,
                          specifications = map (\spec -> (currentVersion, spec)) specs,
                          proofObligations = map (\po -> (currentVersion, po)) pos,
                          warnings = map (\warning -> (currentVersion, warning)) warnings,
                          -- to support other LSP methods in a light-weighted manner
                          loadedVersion = currentVersion,
                          toOffsetMap = SrcLoc.makeToOffset source,
                          semanticTokens = collectHighlighting concrete,
                          idCount = idCount,
                          definitionLinks = collectLocationLinks abstract,
                          hoverInfos = collectHoverInfo elaborated,
                          positionDelta = idDelta,
                          editedVersion = currentVersion
                        }
                logText "  fileState created\n"
                saveFileState filePath fileState
                logText "  fileState updated\n"
                onSuccess
                return $ Right LoadDone
          holes -> do
            lift $ logText "  should dig holes\n"
            let textEdits = map (\r -> rangeToTextEdit r (diggedText r)) holes
            let textDocEdit =
                  LSP.TextDocumentEdit
                    { _textDocument =
                        LSP.OptionalVersionedTextDocumentIdentifier
                          (LSP.filePathToUri filePath)
                          (LSP.InL vfsVersion),
                      _edits = map LSP.InL textEdits
                    }
            lift $ logText "  returning needsEdit response\n"
            return $ LoadNeedsEdit textDocEdit
      logText "load: end\n"
      case result of
        Left response -> return response
        Right response -> return response
  where
    onSuccess :: ServerM ()
    onSuccess = do
      logText "load: success\n"
      sendUpdateNotification filePath
      logText "load: update notification sent\n"
    onError :: Error -> ServerM ()
    onError err = do
      logText "load: error\n\t"
      logText $ Text.pack (show err)
      logText "\n"
      sendErrorNotification filePath [err]
      logText "load: update notification sent\n"

    parse :: FilePath -> Text -> ServerM (Either LoadResponse C.Program)
    parse filepath source =
      case Parser.scanAndParse Parser.program filepath source of
        Left err -> do
          logText "  parse error"
          onError $ ParseError err
          return $ Left LoadDone
        Right concrete -> do
          logText "  source parsed \n"
          return $ Right concrete

    collectHoles :: C.Program -> [Range]
    collectHoles (C.Program _ statements) = collectHolesFromStatements statements

    collectHolesFromStatements :: [C.Stmt] -> [Range]
    collectHolesFromStatements statements = do
      statement <- statements
      case statement of
        C.SpecQM range -> [range]
        C.Block _ program _ -> collectHoles program
        C.Do _ commands _ -> collectHolesFromGdCmd commands
        C.If _ commands _ -> collectHolesFromGdCmd commands
        _ -> []

    mapSepBy :: (a -> b) -> SepBy s a -> [b]
    mapSepBy f (Head c) = [f c]
    mapSepBy f (Delim c _ cs) = f c : mapSepBy f cs

    collectHolesFromGdCmd :: SepBy s C.GdCmd -> [Range]
    collectHolesFromGdCmd s = do
      ranges <- mapSepBy (\(GdCmd _ _ statements) -> collectHolesFromStatements statements) s
      ranges

    elaborate :: A.Program -> ServerM (Either LoadResponse T.Program)
    elaborate abstract = do
      case TypeChecking.runElaboration abstract mempty of
        Left e -> do
          logText "  elaborate error\n"
          onError $ TypeError e
          return $ Left LoadDone
        Right typedProgram -> do
          logText "  program elaborated\n"
          return $ Right typedProgram
