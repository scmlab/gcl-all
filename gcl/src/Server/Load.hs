{-# LANGUAGE OverloadedStrings #-}

module Server.Load where

import Data.Bifunctor (first)
import qualified Data.IntMap as IntMap
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Dependency (evalDependencyResolution)
import GCL.Range (Range, posCol, posLine, rangeEnd, rangeStart)
import GCL.Type2.ToTyped (runToTyped)
import GCL.Type2.Types (Inference (..))
import qualified GCL.WP as WP
import qualified Hack
import Server.GoToDefn (collectLocationLinks)
import Server.Highlighting (collectHighlighting)
import Server.Hover (collectHoverInfo)
import Server.Monad (FileState (..), HoleKind (..), PendingEdit (..), ServerM, emptyFileStateWithErrors, getPendingEdit, logText, logTextLn, readSourceAndVersion, sendEditTextsWithVersion, sendWindowInfoMessage, setFileState, setPendingEdit)
import Server.Notification.Update (sendFileState, sendFileStateWithRefresh)
import qualified Syntax.Concrete as C
import qualified Syntax.Concrete.Instances.ToAbstract as C
import Syntax.Concrete.Types (GdCmd (..), SepBy (..))
import qualified Syntax.Parser as Parser
import Syntax.Parser.Error (ParseError)

--------------------------------------------------------------------------------
-- Types

-- | Result of digging holes: (edits in original coordinates, new source after edits)
type DigResult = ([(Range, Text)], Text)

--------------------------------------------------------------------------------
-- ServerM action

load :: FilePath -> ServerM ()
load filePath = do
  maybePending <- getPendingEdit filePath
  case maybePending of
    Just _ -> do
      logTextLn "Load: pending edit exists, skipping"
      sendWindowInfoMessage "GCL: busy, please retry"
    Nothing -> do
      logText "Load: start\n"
      logText "Load: reading virtual file\n"
      maybeSource <- readSourceAndVersion filePath
      case maybeSource of
        Nothing ->
          logText "Load: cannot read virtual file\n"
        Just (source, vfsVersion) ->
          case loadAndDig filePath source of
            Left parseErr -> do
              logTextLn "Load: parse error"
              let fs = emptyFileStateWithErrors [ParseError parseErr]
              setFileState filePath fs
              sendFileState filePath fs
            Right (maybeDig, eitherFs) ->
              let fs = either (emptyFileStateWithErrors . pure) id eitherFs
               in case maybeDig of
                    Nothing -> do
                      logText "Load: no holes, setting file state directly\n"
                      setFileState filePath fs
                      case eitherFs of
                        Right _ -> do
                          logText "Load: sending refresh\n"
                          sendFileStateWithRefresh filePath fs
                        Left _ -> do
                          logTextLn "Load: type/struct error"
                          sendFileState filePath fs
                    Just (edits, newSource) -> do
                      logText "Load: holes dug, sending edit\n"
                      sendEditTextsWithVersion filePath vfsVersion edits
                      logText "Load: setting pending edit\n"
                      setPendingEdit
                        filePath
                        PendingEdit
                          { expectedContent = newSource,
                            pendingFileState = fs
                          }
      logText "Load: end\n"

--------------------------------------------------------------------------------
-- Main pipeline

-- | Full pipeline: parseAndDig → loadConcrete.
-- Outer Either: parse error (before edit decision).
-- Inner Either: type/struct error (after edit decision).
-- The DigResult and the FileState are independent — a type error does not block edits.
loadAndDig :: FilePath -> Text -> Either ParseError (Maybe DigResult, Either Error FileState)
loadAndDig filePath source = do
  (maybeDig, concrete) <- parseAndDig filePath source
  return (maybeDig, loadConcrete concrete)

-- | Pipeline from concrete AST: abstract → elaborate → sweep.
loadConcrete :: C.Program -> Either Error FileState
loadConcrete concrete = do
  let abstract = C.runAbstractTransform concrete
  abstract' <- first TypeError $ evalDependencyResolution abstract
  (elaborated, Inference c) <- first (TypeError . Hack.toOldError) $ runToTyped abstract' mempty
  (pos, specs, holes, warnings, _redexes, idCount) <- first StructError $ WP.sweep elaborated
  return
    FileState
      { fsErrors = [],
        fsSpecifications = specs,
        fsHoles = holes,
        fsProofObligations = pos,
        fsWarnings = warnings,
        fsIdCount = idCount,
        fsMetaVarIdCount = c,
        fsSemanticTokens = collectHighlighting concrete,
        fsDefinitionLinks = collectLocationLinks abstract,
        fsHoverInfos = collectHoverInfo elaborated
      }

-- | Parse source, and if holes are found, dig them and re-parse.
-- Returns (if holes were dug) the DigResult, and the clean concrete AST.
parseAndDig :: FilePath -> Text -> Either ParseError (Maybe DigResult, C.Program)
parseAndDig filePath source = do
  (concrete1, holes1) <- parseAndCollectHoles filePath source
  case holes1 of
    [] -> Right (Nothing, concrete1)
    _ -> do
      let (edits, newSource) = replaceHoles source holes1
      (concrete2, holes2) <- parseAndCollectHoles filePath newSource
      case holes2 of
        [] -> Right (Just (edits, newSource), concrete2)
        _ -> error "unexpected holes after digging"
  where
    parseAndCollectHoles fp src =
      case Parser.scanAndParse Parser.program fp src of
        Left err -> Left err
        Right concrete -> Right (concrete, collectHoles concrete)
    replaceHoles src holeList = (edits, applyEdits src edits)
      where
        edits = map (\(kind, range) -> (range, diggedText kind range)) holeList

--------------------------------------------------------------------------------
-- Hole digging

-- | Replacement text for a hole, matching the behaviour of Server.Monad.digHoles.
diggedText :: HoleKind -> Range -> Text
diggedText StmtHole range = "[!\n" <> ind <> "\n" <> ind <> "!]"
  where
    ind = Text.replicate (posCol (rangeStart range) - 1) " "
diggedText ExprHole _ = "{! !}"

-- | Apply a list of non-overlapping edits to source text.
-- Edits must not overlap. They are applied using positions in the original source,
-- processed from first to last.
applyEdits :: Text -> [(Range, Text)] -> Text
applyEdits source edits =
  let ls = Text.split (== '\n') source
      lineStarts = IntMap.fromList $ zip [1 ..] $ scanl (\off l -> off + Text.length l + 1) 0 ls
      toOffset pos = IntMap.findWithDefault 0 (posLine pos) lineStarts + (posCol pos - 1)
      sorted = sortBy (comparing (rangeStart . fst)) edits
      go pos [] = Text.drop pos source
      go pos ((range, replacement) : rest) =
        let s = toOffset (rangeStart range)
            e = toOffset (rangeEnd range)
         in Text.take (s - pos) (Text.drop pos source)
              <> replacement
              <> go e rest
   in go 0 sorted

--------------------------------------------------------------------------------
-- Collecting holes from concrete syntax

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

mapSepBy :: (a -> b) -> SepBy s a -> [b]
mapSepBy f (Head c) = [f c]
mapSepBy f (Delim c _ cs) = f c : mapSepBy f cs
