{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use join" #-}

module Server.Load where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Range (Range)
import qualified GCL.Type as TypeChecking
import GCL.Type2.ToTyped
import qualified GCL.WP as WP
import Server.GoToDefn (collectLocationLinks)
import Server.Highlighting (collectHighlighting)
import Server.Hover (collectHoverInfo)
import Server.Monad (FileState (..), HoleKind (..), ServerM, digHoles, increaseDidChangeShouldReload, loadFileState, logText, readSource, saveFileState)
import Server.Notification.Error (sendErrorNotification)
import Server.Notification.Update (sendUpdateNotification)
import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import qualified Syntax.Concrete.Instances.ToAbstract as C
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
    Just source -> do
      -- TODO: we should probably properly separate success and error route
      -- right this returns `Either () ()`
      _ <- runExceptT $ do
        lift $ logText "  source read \n"
        -- parse source into concrete syntax
        concrete <- ExceptT $ parse filePath source
        abstract <- ExceptT $ reportHolesOrToAbstract concrete filePath
        elaborated <- ExceptT $ elaborate2 abstract
        -- elaborated <- ExceptT $ elaborate abstract
        ExceptT $ case WP.sweep elaborated of
          Left err -> do
            logText "  sweep error\n"
            onError (StructError err)
            return $ Left ()
          Right (pos, specs, holes, warnings, redexes, idCount) -> do
            let fileState =
                  FileState
                    { refinedVersion = currentVersion,
                      specifications = map (\spec -> (currentVersion, spec)) specs,
                      holes = map (\hole -> (currentVersion, hole)) holes,
                      proofObligations = map (\po -> (currentVersion, po)) pos,
                      warnings = map (\warning -> (currentVersion, warning)) warnings,
                      didChangeShouldReload = 0,
                      -- to support other LSP methods in a light-weighted manner
                      loadedVersion = currentVersion,
                      semanticTokens = collectHighlighting concrete,
                      idCount = idCount,
                      definitionLinks = collectLocationLinks abstract,
                      hoverInfos = collectHoverInfo elaborated,
                      editedVersion = currentVersion
                    }
            logText "  fileState created\n"
            saveFileState filePath fileState
            logText "  fileState updated\n"
            onSuccess
            return $ Right ()
      return ()
  logText "load: end\n"
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
        [] -> do
          let abstract = C.runAbstractTransform concrete
          logText "  all holes digged\n"
          logText "  abstract program generated\n"
          return $ Right abstract
        holes -> do
          logText "  should dig holes\n"
          digHoles filePath holes do
            logText "  holes digged\n"
            increaseDidChangeShouldReload filePath
          return $ Left ()

    collectHoles :: C.Program -> [(HoleKind, Range)]
    collectHoles (C.Program _ statements) = collectHolesFromStatements statements

    collectHolesFromStatements :: [C.Stmt] -> [(HoleKind, Range)]
    collectHolesFromStatements = concatMap collectHolesFromStatement

    collectHolesFromStatement :: C.Stmt -> [(HoleKind, Range)]
    collectHolesFromStatement (C.SpecQM range) = [(StmtHole, range)]
    collectHolesFromStatement (C.Assign _ _ exprs) = concat $ mapSepBy collectHolesFromExpr exprs
    collectHolesFromStatement (C.AAssign _ _ a _ _ b) = collectHolesFromExpr a ++ collectHolesFromExpr b
    collectHolesFromStatement (C.Assert _ a _) = collectHolesFromExpr a
    collectHolesFromStatement (C.LoopInvariant _ a _ _ _ b _) = collectHolesFromExpr a ++ collectHolesFromExpr b
    collectHolesFromStatement (C.Do _ ss _) = concat $ mapSepBy collectHolesFromGdCmd ss
    collectHolesFromStatement (C.If _ ss _) = concat $ mapSepBy collectHolesFromGdCmd ss
    collectHolesFromStatement (C.Alloc _ _ _ _ es _) = concat $ mapSepBy collectHolesFromExpr es
    collectHolesFromStatement (C.HLookup _ _ _ a) = collectHolesFromExpr a
    collectHolesFromStatement (C.HMutate _ a _ b) = collectHolesFromExpr a ++ collectHolesFromExpr b
    collectHolesFromStatement (C.Dispose _ a) = collectHolesFromExpr a
    collectHolesFromStatement _ = []

    mapSepBy :: (a -> b) -> SepBy s a -> [b]
    mapSepBy f (Head c) = [f c]
    mapSepBy f (Delim c _ cs) = f c : mapSepBy f cs

    collectHolesFromGdCmd :: C.GdCmd -> [(HoleKind, Range)]
    collectHolesFromGdCmd (GdCmd _ _ stmts) = collectHolesFromStatements stmts

    collectHolesFromExpr :: C.Expr -> [(HoleKind, Range)]
    collectHolesFromExpr (C.HoleQM range) = [(ExprHole, range)]
    collectHolesFromExpr (C.Paren _ expr _) = collectHolesFromExpr expr
    collectHolesFromExpr (C.Arr a _ b _) = collectHolesFromExpr a ++ collectHolesFromExpr b
    collectHolesFromExpr (C.App a b) = collectHolesFromExpr a ++ collectHolesFromExpr b
    collectHolesFromExpr (C.Quant _ _ _ _ a _ b _) = collectHolesFromExpr a ++ collectHolesFromExpr b
    collectHolesFromExpr (C.Case _ a _ _) = collectHolesFromExpr a
    collectHolesFromExpr _ = []

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

    elaborate2 :: A.Program -> ServerM (Either () T.Program)
    elaborate2 abstract = do
      logText "  [WARN] running experimental new typechecker, things might break"
      case runToTyped abstract mempty of
        Left e -> do
          logText "  elaborate error\n"
          onError $ TypeError e
          return $ Left ()
        Right typedProgram -> do
          logText "  program elaborated\n"
          return $ Right typedProgram
