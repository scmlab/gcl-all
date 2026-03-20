{-# LANGUAGE OverloadedStrings #-}

module Server.Refine where

import Control.Monad (when)
import Data.Bifunctor (first, second)
import Data.List (find)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Common (Index, TypeInfo)
import GCL.Predicate (Hole (..), InfMode (..), PO (..), Spec (..))
import GCL.Range (Pos (..), R (..), Range (..), extractText, mkPos, mkRange, rangeStart)
import GCL.Type (runElaboration)
import GCL.WP (collectStmtHoles, runWP, structStmts)
import GCL.WP.Types (StructError, StructWarning (..))
import Language.Lexer.Applicative (TokenStream (..))
import Server.Highlighting (collectHighlightingFromStmts)
import Server.Hover (collectHoverInfoFromStmts)
import Server.Load (applyEdits, collectHolesFromStatements, diggedText)
import Server.Monad
  ( FileState (..),
    PendingEdit (..),
    ServerM,
    getFileState,
    logText,
    logTextLn,
    readSourceAndVersion,
    sendEditTextsWithVersion,
    setPendingEdit,
  )
import Server.Move (applyMovesToFileState, mkLSPMove)
import Server.Notification.Error (sendErrorNotification)
import Server.SrcLoc (toLSPRange)
import qualified Syntax.Concrete as C
import qualified Syntax.Concrete.Instances.ToAbstract as C
import qualified Syntax.Parser as Parser
import Syntax.Parser.Error (ParseError (..))
import Syntax.Parser.Lexer (TokStream, scan)
import qualified Syntax.Typed as T

--------------------------------------------------------------------------------
-- ServerM action

refine :: FilePath -> Pos -> ServerM ()
refine filePath cursor = do
  logTextLn $ "Refine: cursor: " <> Text.pack (show cursor)
  maybeFs <- getFileState filePath
  maybeSource <- readSourceAndVersion filePath
  let result = do
        fs <- maybe (Left [Others "Refine" "File not loaded." Nothing]) Right maybeFs
        (source, vfsVersion) <- maybe (Left [Others "Refine" "Cannot read source." Nothing]) Right maybeSource
        spec <- maybe (Left [Others "Refine" "No enclosing spec found." Nothing]) Right (findEnclosingSpec cursor (fsSpecifications fs))
        (finalImplText, eitherFs) <- first pure $ refineAndDig filePath (fsIdCount fs) spec source
        return (fs, source, vfsVersion, spec, finalImplText, eitherFs)
  case result of
    Left errs -> sendErrorNotification filePath errs
    Right (fs, source, vfsVersion, spec, finalImplText, eitherFs) -> do
      sendEditTextsWithVersion filePath vfsVersion [(specRange spec, finalImplText)]
      logText "Refine: edit sent\n"
      case eitherFs of
        Left err -> sendErrorNotification filePath [err]
        Right fragmentFs -> do
          let lspMove = mkLSPMove (toLSPRange (specRange spec)) finalImplText
              newFs = mergeFileState (applyMovesToFileState [lspMove] fs) fragmentFs
              newSource = applyEdits source [(specRange spec, finalImplText)]
              pending = PendingEdit {expectedContent = newSource, pendingFileState = newFs}
          setPendingEdit filePath pending
          sendErrorNotification filePath []

--------------------------------------------------------------------------------
-- Main pipeline

-- | Full pipeline: validate spec → extract impl → parseAndDigFragment → loadConcreteFragment.
-- Outer Either: validation/parse error (before edit decision).
-- Inner Either: type/struct error (after edit decision).
-- The finalImplText and the FileState are independent — a type error does not block edits.
refineAndDig :: FilePath -> Int -> Spec -> Text -> Either Error (Text, Either Error FileState)
refineAndDig filePath idCount spec source = do
  let specRng = specRange spec
  when (isSingleLine specRng) $
    Left (Others "Refine" "Spec should have more than one line." (Just specRng))
  let implRange = shrinkRange 2 specRng
      implText = extractText implRange source
  when (not (isFirstLineBlank implText)) $
    Left (Others "Refine" "The first line in the spec must be blank." (Just implRange))
  let implStart = rangeStart implRange

  (finalImplText, stmts) <- parseAndDigFragment filePath implStart implText
  return (finalImplText, loadConcreteFragment (specTypeEnv spec) idCount spec stmts)

-- | Pipeline from concrete stmts: abstract → elaborate → sweep.
loadConcreteFragment :: [(Index, TypeInfo)] -> Int -> Spec -> [C.Stmt] -> Either Error FileState
loadConcreteFragment typeEnv idCount spec stmts = do
  let abstract = C.runAbstractTransform stmts
  elaborated <- first TypeError $ runElaboration abstract typeEnv
  (pos, specs, holes, warnings, idCount') <-
    first StructError $ sweepFragment idCount spec elaborated
  return
    FileState
      { fsSpecifications = specs,
        fsHoles = holes,
        fsProofObligations = pos,
        fsWarnings = warnings,
        fsIdCount = idCount',
        fsSemanticTokens = collectHighlightingFromStmts stmts,
        fsDefinitionLinks = mempty, -- TODO: needs scope info from declarations
        fsHoverInfos = collectHoverInfoFromStmts elaborated
      }

-- | Parse fragment and dig holes if needed.
-- Internally uses relative positions (1,1) for hole digging,
-- then absolute positions (fragmentStart) for the returned stmts.
-- Returns (finalImplText, stmts).
parseAndDigFragment :: FilePath -> Pos -> Text -> Either Error (Text, [C.Stmt])
parseAndDigFragment filePath fragmentStart implText = do
  -- TODO: reduce parsing
  (_, holes1) <- parseRelative implText
  case holes1 of
    [] -> do
      stmts <- parseAbsolute implText
      Right (implText, stmts)
    _ -> do
      let newImplText = digHoles implText holes1
      stmts2 <- parseAbsolute newImplText
      let holes2 = collectHolesFromStatements stmts2
      case holes2 of
        [] -> Right (newImplText, stmts2)
        _ -> Left (Others "Refine" "unexpected holes after digging" Nothing)
  where
    parseRelative src = do
      stmts <- parseFragmentStmts filePath (mkPos 1 1) src
      return (stmts, collectHolesFromStatements stmts)
    parseAbsolute src =
      parseFragmentStmts filePath fragmentStart src
    digHoles src holeList =
      let edits = map (\(kind, range) -> (range, diggedText kind range)) holeList
       in applyEdits src edits

--------------------------------------------------------------------------------
-- Fragment parsing

-- | Parse text as a list of statements with position translation.
parseFragmentStmts :: FilePath -> Pos -> Text -> Either Error [C.Stmt]
parseFragmentStmts filePath fragmentStart src =
  let tokens = scan filePath src
      tokens' = translateTokStream fragmentStart tokens
   in case Parser.parse Parser.statements filePath tokens' of
        Left (errors, logMsg) -> Left (ParseError (SyntacticError errors logMsg))
        Right stmts -> Right stmts

translateTokStream :: Pos -> TokStream -> TokStream
translateTokStream start (TsToken (R range x) rest) =
  TsToken (R (translateTokenRange start range) x) (translateTokStream start rest)
translateTokStream _ TsEof = TsEof
translateTokStream _ (TsError e) = TsError e

translateTokenRange :: Pos -> Range -> Range
translateTokenRange start (Range left right) =
  mkRange (translatePos start left) (translatePos start right)

translatePos :: Pos -> Pos -> Pos
translatePos (Pos lineStart colStart) (Pos lineOff colOff) =
  mkPos
    (lineStart + lineOff - 1)
    (if lineOff == 1 then colStart + colOff - 1 else colOff)

--------------------------------------------------------------------------------
-- Sweep fragment

-- | Sweep a fragment using the spec's pre/post conditions.
sweepFragment :: Int -> Spec -> [T.Stmt] -> Either StructError ([PO], [Spec], [Hole], [StructWarning], Int)
sweepFragment counter (Specification _ pre post _ _) impl =
  second
    ( \(_, counter', (pos, specs, sws, _redexes)) ->
        (pos, specs, concatMap collectStmtHoles impl, sws, counter')
    )
    $ runWP
      (structStmts Primary (pre, Nothing) impl post)
      (Map.empty, [])
      counter

--------------------------------------------------------------------------------
-- Helpers

isSingleLine :: Range -> Bool
isSingleLine (Range (Pos l1 _) (Pos l2 _)) = l1 == l2

shrinkRange :: Int -> Range -> Range
shrinkRange diff (Range (Pos l1 c1) (Pos l2 c2)) =
  mkRange (mkPos l1 (c1 + diff)) (mkPos l2 (c2 - diff))

isFirstLineBlank :: Text -> Bool
isFirstLineBlank = Text.null . Text.strip . Text.takeWhile (/= '\n')

--------------------------------------------------------------------------------
-- Finding enclosing spec

findEnclosingSpec :: Pos -> [Spec] -> Maybe Spec
findEnclosingSpec cursor specs =
  find (\spec -> isInRange cursor (shrinkRange 1 (specRange spec))) specs
  where
    isInRange (Pos l c) (Range (Pos l1 c1) (Pos l2 c2)) =
      (l1, c1) <= (l, c) && (l, c) <= (l2, c2)

--------------------------------------------------------------------------------
-- Merge FileState

-- | Merge a fragment FileState (from refine) into the already-moved base state.
mergeFileState :: FileState -> FileState -> FileState
mergeFileState moved fragment =
  FileState
    { fsSpecifications = fsSpecifications moved ++ fsSpecifications fragment,
      fsHoles = fsHoles moved ++ fsHoles fragment,
      fsProofObligations = fsProofObligations moved ++ fsProofObligations fragment,
      fsWarnings = fsWarnings moved ++ fsWarnings fragment,
      fsIdCount = fsIdCount fragment,
      fsSemanticTokens = fsSemanticTokens moved ++ fsSemanticTokens fragment,
      fsDefinitionLinks = fsDefinitionLinks moved <> fsDefinitionLinks fragment,
      fsHoverInfos = fsHoverInfos moved <> fsHoverInfos fragment
    }
