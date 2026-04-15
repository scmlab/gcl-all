{-# LANGUAGE OverloadedStrings #-}

module Server.Refine where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (unless, when)
import Data.Bifunctor (Bifunctor (bimap), first, second)
import Data.List (find)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Predicate (Hole (..), InfMode (..), PO (..), Spec (..))
import GCL.Range (Pos (..), R (..), Range (..), extractText, mkPos, mkRange, rangeStart)
import GCL.Type2.Infer (typeCheck')
import GCL.Type2.ToTyped (runToTyped)
import GCL.Type2.Types (Env, Inference (Inference), runTI)
import GCL.WP (collectExprHoles, collectStmtHoles, runWP, structStmts)
import GCL.WP.Types (StructError, StructWarning (..))
import qualified Hack
import Language.Lexer.Applicative (TokenStream (..))
import Pretty.Error ()
import Prettyprinter (layoutCompact, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Server.Highlighting (collectHighlightingFromStmts)
import Server.Hover (collectHoverInfoFromStmts)
import Server.Load (applyEdits, collectHolesFromExpr, collectHolesFromStatements, diggedText)
import Server.Monad
  ( FileState (..),
    PendingEdit (..),
    ServerM,
    getFileState,
    getPendingEdit,
    logText,
    logTextLn,
    readSourceAndVersion,
    sendEditTextsWithVersion,
    sendWindowInfoMessage,
    setPendingEdit,
  )
import Server.Move (applyMovesToFileState, mkLSPMove)
import Server.SrcLoc (toLSPRange)
import qualified Syntax.Abstract as A
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
        enclosingTarget <- maybe (Left [Others "Refine" "No enclosing spec or hole found." Nothing]) Right (findSpecOrHole cursor (fsSpecifications fs) (fsHoles fs))
        return (fs, source, vfsVersion, enclosingTarget)

  case result of
    Left errs -> sendWindowInfoMessage (Text.intercalate "\n" $ map (renderStrict . layoutCompact . pretty) errs)
    Right (fs, source, vfsVersion, Left spec) -> do
      case refineAndDig filePath (fsIdCount fs) spec source of
        Left errs -> sendWindowInfoMessage (Text.intercalate "\n" [(renderStrict . layoutCompact . pretty) errs])
        Right (finalImplText, eitherFs) -> do
          sendEditTextsWithVersion filePath vfsVersion [(specRange spec, finalImplText)]
          logText "Refine: spec edit sent\n"
          let lspMove = mkLSPMove (toLSPRange (specRange spec)) finalImplText
              movedFs = applyMovesToFileState [lspMove] fs
              newSource = applyEdits source [(specRange spec, finalImplText)]
          case eitherFs of
            Left err -> do
              let pendingFs = movedFs {fsErrors = fsErrors movedFs ++ [err]}
                  pending = PendingEdit {expectedContent = newSource, pendingFileState = pendingFs}
              setPendingEdit filePath pending
            Right fragmentFs -> do
              let newFs = mergeFileState movedFs fragmentFs
                  pending = PendingEdit {expectedContent = newSource, pendingFileState = newFs}
              setPendingEdit filePath pending
    Right (fs, source, vfsVersion, Right hole) -> do
      case refineHoleAndDig filePath hole source (fsMetaVarIdCount fs) of
        Left err -> sendWindowInfoMessage (Text.intercalate "\n" [(renderStrict . layoutCompact . pretty) err])
        Right (finalImplText, (typedExpr, state)) -> do
          sendEditTextsWithVersion filePath vfsVersion [(holeRange hole, finalImplText)]
          logText "Refine: hole edit sent\n"
          let lspMove = mkLSPMove (toLSPRange (holeRange hole)) finalImplText
              (holes1, holes2) = splitAtFirst hole (fsHoles fs)
              newHoles = justifyExpHoleRanges $ collectExprHoles typedExpr
              holes2' = justifyRearHoleRanges (length newHoles - 1) (holeRange hole) holes2
              newFs =
                (applyMovesToFileState [lspMove] fs)
                  { fsHoles = updateHoleIds (holes1 <> newHoles <> holes2'),
                    fsTIState = state
                  }
              newSource = applyEdits source [(holeRange hole, finalImplText)]
              pending = PendingEdit {expectedContent = newSource, pendingFileState = newFs}
          setPendingEdit filePath pending
  where
    splitAtFirst :: (Eq a) => a -> [a] -> ([a], [a])
    splitAtFirst x = fmap (drop 1) . break (x ==)

    updateHoleIds :: [Hole] -> [Hole]
    updateHoleIds = zipWith (\idx hole -> hole {holeID = idx}) [0 ..]

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
  unless (isFirstLineBlank implText) $
    Left (Others "Refine" "The first line in the spec must be blank." (Just implRange))
  let implStart = rangeStart implRange

  (finalImplText, stmts) <- parseAndDigFragment filePath implStart implText
  return (finalImplText, loadConcreteFragment (specTypeEnv spec) idCount spec stmts)

refineHoleAndDig :: FilePath -> Hole -> Text -> Inference -> Either Error (Text, (T.Expr, Inference))
refineHoleAndDig filePath hole source state = do
  let holeRng = holeRange hole
  unless (isSingleLine holeRng) $
    Left (Others "Refine" "Hole should have exact one line." (Just holeRng))
  let implRange = shrinkRange 2 holeRng
      implText = Text.strip $ extractText implRange source
      implStart = rangeStart implRange

  (finalImplText, expr) <- parseAndDigHoleFragment filePath implStart implText
  typedExpr <- loadConcreteHoleFragment (holeTypeEnv hole) (holeType hole) expr state
  return (finalImplText, typedExpr)

-- | Pipeline from concrete stmts: abstract → elaborate → sweep.
loadConcreteFragment :: Env -> Int -> Spec -> [C.Stmt] -> Either Error FileState
loadConcreteFragment typeEnv idCount spec stmts = do
  let abstract = C.runAbstractTransform stmts
  result <- first (TypeError . Hack.toOldError) (mapM (`runToTyped` typeEnv) abstract)
  let (elaborated, state) = foldTIResult result
  (pos, specs, holes, warnings, idCount') <-
    first StructError $ sweepFragment idCount spec elaborated
  return
    FileState
      { fsErrors = [],
        fsSpecifications = specs,
        fsHoles = holes,
        fsProofObligations = pos,
        fsWarnings = warnings,
        fsIdCount = idCount',
        fsTIState = state,
        fsSemanticTokens = collectHighlightingFromStmts stmts,
        fsDefinitionLinks = mempty, -- TODO: needs scope info from declarations
        fsHoverInfos = collectHoverInfoFromStmts elaborated
      }
  where
    foldTIResult :: [(T.Stmt, Inference)] -> ([T.Stmt], Inference)
    foldTIResult = second maximum . unzip

loadConcreteHoleFragment :: Env -> A.Type -> C.Expr -> Inference -> Either Error (T.Expr, Inference)
loadConcreteHoleFragment typeEnv ty expr state = do
  let abstract = C.runAbstractTransform expr
  bimap (TypeError . Hack.toOldError) (first snd) (runTI (typeCheck' abstract ty) typeEnv state)

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
    parseAbsolute = parseFragmentStmts filePath fragmentStart
    digHoles src holeList =
      let edits = map (\(kind, range) -> (range, diggedText kind range)) holeList
       in applyEdits src edits

parseAndDigHoleFragment :: FilePath -> Pos -> Text -> Either Error (Text, C.Expr)
parseAndDigHoleFragment filePath fragmentStart implText = do
  (_, holes1) <- parseRelative implText
  case holes1 of
    [] -> do
      expr <- parseAbsolute implText
      Right (implText, expr)
    _ -> do
      let newImplText = digHoles implText holes1
      exprs2 <- parseAbsolute newImplText
      let holes2 = collectHolesFromExpr exprs2
      case holes2 of
        [] -> Right (newImplText, exprs2)
        _ -> Left (Others "Refine" "unexpected holes after digging" Nothing)
  where
    parseRelative src = do
      expr <- parseFragmentExpr filePath (mkPos 1 1) src
      return (expr, collectHolesFromExpr expr)
    parseAbsolute = parseFragmentExpr filePath fragmentStart
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

parseFragmentExpr :: FilePath -> Pos -> Text -> Either Error C.Expr
parseFragmentExpr filePath fragmentStart src =
  let tokens = scan filePath src
      tokens' = translateTokStream fragmentStart tokens
   in case Parser.parse Parser.expression filePath tokens' of
        Left (errors, logMsg) -> Left (ParseError (SyntacticError errors logMsg))
        Right expr -> Right expr

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

onSameLine :: Range -> Range -> Bool
onSameLine (Range (Pos l1 _) (Pos l2 _)) (Range (Pos l3 _) (Pos l4 _)) = l1 == l3 && l2 == l4

shrinkRange :: Int -> Range -> Range
shrinkRange diff (Range (Pos l1 c1) (Pos l2 c2)) =
  mkRange (mkPos l1 (c1 + diff)) (mkPos l2 (c2 - diff))

isFirstLineBlank :: Text -> Bool
isFirstLineBlank = Text.null . Text.strip . Text.takeWhile (/= '\n')

justifyExpHoleRanges :: [Hole] -> [Hole]
justifyExpHoleRanges = map (justifyHoleRange (-2))

justifyRearHoleRanges :: Int -> Range -> [Hole] -> [Hole]
justifyRearHoleRanges expDiff r = map (\hole@(Hole _ _ hr _) -> if onSameLine r hr then justifyHoleRange (expDiff * 4 - 1) hole else hole)

justifyHoleRange :: Int -> Hole -> Hole
justifyHoleRange diff hole@(Hole _ _ (Range (Pos l1 c1) (Pos l2 c2)) _) =
  hole {holeRange = mkRange (mkPos l1 (c1 + diff)) (mkPos l2 (c2 + diff))}

--------------------------------------------------------------------------------
-- Finding enclosing spec

findSpecOrHole :: Pos -> [Spec] -> [Hole] -> Maybe (Either Spec Hole)
findSpecOrHole cursor specs holes = (Left <$> findByCursor specRange specs) <|> (Right <$> findByCursor holeRange holes)
  where
    findByCursor :: (Foldable t) => (a -> Range) -> t a -> Maybe a
    findByCursor f = find (isInRange cursor . shrinkRange 1 . f)

    isInRange :: Pos -> Range -> Bool
    isInRange (Pos l c) (Range (Pos l1 c1) (Pos l2 c2)) =
      (l1, c1) <= (l, c) && (l, c) <= (l2, c2)

--------------------------------------------------------------------------------
-- Merge FileState

-- | Merge a fragment FileState (from refine) into the already-moved base state.
mergeFileState :: FileState -> FileState -> FileState
mergeFileState moved fragment =
  FileState
    { fsErrors = fsErrors moved, -- Keep previous errors
      fsSpecifications = fsSpecifications moved ++ fsSpecifications fragment,
      fsHoles = fsHoles moved ++ fsHoles fragment,
      fsProofObligations = fsProofObligations moved ++ fsProofObligations fragment,
      fsWarnings = fsWarnings moved ++ fsWarnings fragment,
      fsIdCount = fsIdCount fragment,
      fsTIState = max (fsTIState moved) (fsTIState fragment),
      fsSemanticTokens = fsSemanticTokens moved ++ fsSemanticTokens fragment,
      fsDefinitionLinks = fsDefinitionLinks moved <> fsDefinitionLinks fragment,
      fsHoverInfos = fsHoverInfos moved <> fsHoverInfos fragment
    }
