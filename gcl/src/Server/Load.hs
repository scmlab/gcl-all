{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use join" #-}

module Server.Load where

import Control.Monad.Except (ExceptT (ExceptT), runExcept, runExceptT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as JSON
import GCL.Range (Range)
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Predicate (PO, Spec)
import qualified GCL.Type as TypeChecking
import qualified GCL.WP as WP
import GCL.WP.Types (StructError, StructWarning)
import Pretty (Pretty (..))
import Server.GoToDefn (collectLocationLinks)
import Server.Highlighting (collectHighlighting)
import Server.Hover (collectHoverInfo)
import Server.Monad (FileState (..), ServerM, digHoles, increaseDidChangeShouldReload, loadFileState, logText, readSource, saveFileState)
import Server.Notification.Error (sendErrorNotification)
import Server.Notification.Update (sendUpdateNotification)
import Server.PositionMapping (idDelta)
import qualified Server.SrcLoc as SrcLoc
import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import Syntax.Concrete.Types (GdCmd (..), SepBy (..))
import qualified Syntax.Parser as Parser
import qualified Syntax.Typed as T

load :: FilePath -> ServerM ()
load filePath = do
  logText "load: start\n"
  maybeFileState <- loadFileState filePath
  let currentVersion = case maybeFileState of
        Nothing -> 0
        Just FileState {editedVersion} -> editedVersion

  -- read source
  maybeSource <- readSource filePath
  case maybeSource of
    Nothing -> do
      logText "  read error"
      onError (CannotReadFile filePath)
    Just source -> undefined runExceptT $ do
      lift $ logText "  source read \n"
      -- parse source into concrete syntax
      concrete <- ExceptT $ parse filePath source
      abstract <- ExceptT $ reportHolesOrToAbstract concrete filePath
      elaborated <- ExceptT $ elaborate abstract
      ExceptT $ case WP.sweep elaborated of
        Left err -> do
          logText "  sweep error\n"
          onError (StructError err)
          return $ Left ()
        Right (pos, specs, warnings, redexes, idCount) -> do
          let fileState =
                FileState
                  { refinedVersion = currentVersion,
                    specifications = map (\spec -> (currentVersion, spec)) specs,
                    proofObligations = map (\po -> (currentVersion, po)) pos,
                    warnings = map (\warning -> (currentVersion, warning)) warnings,
                    didChangeShouldReload = 0,
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
          return $ Right ()
  logText "load: end\n"
  where
    onSuccess :: ServerM ()
    onSuccess = do
      logText "load: success\n"
      sendUpdateNotification filePath
      -- clear errors
      sendErrorNotification filePath []
      logText "load: update notification sent\n"
    onError :: Error -> ServerM ()
    onError err = do
      logText "load: error\n\t"
      logText $ Text.pack (show err)
      logText "\n"
      sendErrorNotification filePath [err]
      logText "load: update notification sent\n"

    parse :: FilePath -> Text -> ServerM (Either () C.Program)
    parse filepath source =
      case Parser.scanAndParse Parser.program filepath source of
        Left err -> do
          logText "  parse error"
          onError $ ParseError err
          return $ Left ()
        Right concrete -> do
          logText "  source parsed \n"
          return $ Right concrete

    reportHolesOrToAbstract :: C.Program -> FilePath -> ServerM (Either () A.Program)
    reportHolesOrToAbstract concrete filePath =
      case collectHoles concrete of
        [] -> case runExcept $ C.toAbstract concrete of
          Left _ -> do
            logText . Text.pack $ show concrete
            error "should dig all holes before calling Concrete.toAbstract"
          Right abstract -> do
            logText "  all holes digged\n"
            logText "  abstract program generated\n"
            return $ Right abstract
        holes -> do
          logText "  should dig holes\n"
          digHoles filePath holes do
            logText "  holes digged\n"
            increaseDidChangeShouldReload filePath
          return $ Left ()

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

    elaborate :: A.Program -> ServerM (Either () T.Program)
    elaborate abstract = do
      case TypeChecking.runElaboration abstract mempty of
        Left e -> do
          logText "  elaborate error\n"
          onError $ TypeError e
          return $ Left ()
        Right typedProgram -> do
          logText "  program elaborated\n"
          return $ Right typedProgram
