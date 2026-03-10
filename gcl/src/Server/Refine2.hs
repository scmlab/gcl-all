{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Refine2 where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Bifunctor (first, second)
import Data.Int (Int32)
import Data.List (find)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Common (Index, TypeInfo)
import GCL.Predicate (Hole (..), InfMode (..), PO (..), Spec (..))
import GCL.Range (Pos (..), R (..), Range (..), extractText, mkPos, mkRange, rangeStart)
import GCL.Type (Elab (..), Typed, runElaboration)
import GCL.WP (collectStmtHoles, runWP, structStmts)
import GCL.WP.Types (StructError, StructWarning (..))
import Language.Lexer.Applicative (TokenStream (..))
import Server.Change (mkLSPMove)
import Server.Highlighting (collectHighlightingFromStmts)
import Server.Hover (collectHoverInfoFromStmts)
import Server.Load2 (applyEdits, collectHolesFromStatements, diggedText)
import Server.Monad
  ( FileState3 (..),
    PendingEdit (..),
    ServerM,
    editTextsWithVersion,
    getFileState3,
    logText,
    logTextLn,
    readSourceAndVersion,
    setPendingEdit,
    translateFileState3,
  )
import Server.Notification.Error (sendErrorNotification)
import Server.SrcLoc (toLSPRange)
import qualified Syntax.Concrete as C
import qualified Syntax.Concrete.Instances.ToAbstract as C
import qualified Syntax.Parser as Parser
import Syntax.Parser.Error (ParseError (..))
import Syntax.Parser.Lexer (TokStream, scan)
import qualified Syntax.Abstract as A
import qualified Syntax.Typed as T

--------------------------------------------------------------------------------
-- ServerM action

refine2 :: FilePath -> Pos -> ServerM ()
refine2 filePath cursor = do
  logTextLn $ "Refine2: cursor: " <> Text.pack (show cursor)
  result <- runExceptT $ do
    fs3 <- getFileState3OrThrow filePath
    (source, vfsVersion) <- readSourceOrThrow filePath
    spec <- findSpecOrThrow cursor (fs3Specifications fs3)
    (finalImplText, fragmentFs3) <- refineOrThrow filePath (fs3IdCount fs3) spec source
    return (fs3, vfsVersion, spec, source, finalImplText, fragmentFs3)
  case result of
    Left errs ->
      sendErrorNotification filePath errs
    Right (fs3, vfsVersion, spec, source, finalImplText, fragmentFs3) -> do
      let lspMove = mkLSPMove (toLSPRange (specRange spec)) finalImplText
          newFs3 = mergeFileState3 (translateFileState3 [lspMove] fs3) fragmentFs3
          newSource = applyEdits source [(specRange spec, finalImplText)]
          pending = PendingEdit {expectedContent = newSource, pendingFileState = newFs3}
      setPendingEdit filePath pending
      editTextsWithVersion filePath vfsVersion [(specRange spec, finalImplText)]
      sendErrorNotification filePath []
      logText "Refine2: edit sent\n"
  where
    getFileState3OrThrow :: FilePath -> ExceptT [Error] ServerM FileState3
    getFileState3OrThrow fp = do
      lift $ logText "Refine2: getting file state\n"
      result <- lift (getFileState3 fp)
      case result of
        Nothing -> throwE [Others "Refine2" "File not loaded." Nothing]
        Just fs3 -> return fs3

    readSourceOrThrow :: FilePath -> ExceptT [Error] ServerM (Text, Int32)
    readSourceOrThrow fp = do
      lift $ logText "Refine2: reading source\n"
      result <- lift (readSourceAndVersion fp)
      case result of
        Nothing -> throwE [Others "Refine2" "Cannot read source." Nothing]
        Just source -> return source

    findSpecOrThrow :: Pos -> [Spec] -> ExceptT [Error] ServerM Spec
    findSpecOrThrow cur specs =
      case findEnclosingSpec cur specs of
        Nothing -> throwE [Others "Refine2" "No enclosing spec found." Nothing]
        Just spec -> return spec

    refineOrThrow :: FilePath -> Int -> Spec -> Text -> ExceptT [Error] ServerM (Text, FileState3)
    refineOrThrow fp idCount spec source =
      case refineAndDig fp idCount spec source of
        Left err -> throwE [err]
        Right result -> return result

--------------------------------------------------------------------------------
-- Main pipeline

-- | Full pipeline: validate spec → extract impl → parseAndDigFragment → loadConcreteFragment.
refineAndDig :: FilePath -> Int -> Spec -> Text -> Either Error (Text, FileState3)
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
  result <- loadConcreteFragment (specTypeEnv spec) idCount spec stmts
  return (finalImplText, result)

-- | Pipeline from concrete stmts: abstract → elaborate → sweep.
loadConcreteFragment :: [(Index, TypeInfo)] -> Int -> Spec -> [C.Stmt] -> Either Error FileState3
loadConcreteFragment typeEnv idCount spec stmts = do
  let abstract = C.runAbstractTransform stmts
  elaborated <- first TypeError $ runElaboration abstract typeEnv
  (pos, specs, holes, warnings, idCount') <-
    first StructError $ sweepFragment idCount spec elaborated
  return
    FileState3
      { fs3Specifications = specs,
        fs3Holes = holes,
        fs3ProofObligations = pos,
        fs3Warnings = warnings,
        fs3IdCount = idCount',
        fs3SemanticTokens = collectHighlightingFromStmts stmts,
        fs3DefinitionLinks = mempty, -- TODO: needs scope info from declarations
        fs3HoverInfos = collectHoverInfoFromStmts elaborated
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
        _ -> Left (Others "Refine2" "unexpected holes after digging" Nothing)
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
-- Merge FileState3

-- | Merge a fragment FileState3 (from refine) into the already-moved base state.
mergeFileState3 :: FileState3 -> FileState3 -> FileState3
mergeFileState3 moved fragment =
  FileState3
    { fs3Specifications = fs3Specifications moved ++ fs3Specifications fragment,
      fs3Holes = fs3Holes moved ++ fs3Holes fragment,
      fs3ProofObligations = fs3ProofObligations moved ++ fs3ProofObligations fragment,
      fs3Warnings = fs3Warnings moved ++ fs3Warnings fragment,
      fs3IdCount = fs3IdCount fragment,
      fs3SemanticTokens = fs3SemanticTokens moved ++ fs3SemanticTokens fragment,
      fs3DefinitionLinks = fs3DefinitionLinks moved <> fs3DefinitionLinks fragment,
      fs3HoverInfos = fs3HoverInfos moved <> fs3HoverInfos fragment
    }

--------------------------------------------------------------------------------
-- Orphan instance

instance Elab [A.Stmt] where
  elaborate stmts env = do
    typed <-
      mapM
        ( \stmt -> do
            (_, typed, _) <- elaborate stmt env
            return typed
        )
        stmts
    return (Nothing, typed, mempty)
