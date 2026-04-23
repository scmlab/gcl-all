{-# LANGUAGE OverloadedStrings #-}

module Server.Refine where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (unless, when)
import Data.Bifunctor (Bifunctor (bimap), first, second)
import Data.Char (isSpace)
import Data.List (find)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Predicate (Hole (..), InfMode (..), PO (..), Spec (..))
import GCL.Range (Pos (..), R (..), Range (..), extractText, mkPos, mkRange, posCol, posLine, rangeEnd, rangeStart)
import GCL.Type2.Infer (typeCheck')
import GCL.Type2.ToTyped (runToTyped)
import GCL.Type2.Types (Env, Inference, runTI)
import GCL.WP (collectTypedHole, runWP, structStmts)
import GCL.WP.Types (StructError, StructWarning (..))
import qualified Hack
import Language.Lexer.Applicative (TokenStream (..))
import Pretty.Error ()
import Prettyprinter (layoutCompact, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Server.Highlighting (collectHighlightingFromStmts)
import Server.Hover (collectHoverInfoFromStmts)
import Server.Load (applyEdits, collectHole, diggedText)
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
    setFileState,
    setPendingEdit,
  )
import Server.Move (applyMovesToFileState, mkLSPMove)
import Server.Notification.Update (sendFileState)
import Server.OrigCoord (convertError, prepareEdits)
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
  maybePending <- getPendingEdit filePath
  case maybePending of
    Just _ -> do
      logTextLn "Refine: pending edit exists, skipping"
      sendWindowInfoMessage "GCL: busy, please retry"
    Nothing -> do
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
            Right (finalImplText, innerEditsRel, eitherFs) -> do
              let edits = [(specRange spec, finalImplText)]
              case eitherFs of
                Left err -> do
                  logTextLn "Refine: spec error, not sending edit"
                  let ers = prepareEdits (decomposeSpecEdits (specRange spec) innerEditsRel)
                      convertedErr = convertError ers err
                      newFs = fs {fsErrors = [convertedErr]}
                  setFileState filePath newFs
                  sendFileState filePath newFs
                Right fragmentFs -> do
                  sendEditTextsWithVersion filePath vfsVersion edits
                  logText "Refine: spec edit sent\n"
                  let lspMove = mkLSPMove (toLSPRange (specRange spec)) finalImplText
                      movedFs = applyMovesToFileState [lspMove] fs
                      newSource = applyEdits source edits
                      newFs = mergeFileState movedFs fragmentFs
                      pending = PendingEdit {expectedContent = newSource, pendingFileState = newFs}
                  setPendingEdit filePath pending
        Right (fs, source, vfsVersion, Right hole) -> do
          case refineHoleAndDig filePath hole source (fsTIState fs) of
            Left err -> sendWindowInfoMessage (Text.intercalate "\n" [(renderStrict . layoutCompact . pretty) err])
            Right (finalImplText, innerEditsRel, eitherResult) -> do
              let edits = [(holeRange hole, finalImplText)]
              case eitherResult of
                Left err -> do
                  logTextLn "Refine: hole error, not sending edit"
                  let origContent = extractText (shrinkRange 2 (holeRange hole)) source
                      ers = prepareEdits (decomposeHoleEdits (holeRange hole) origContent innerEditsRel)
                      convertedErr = convertError ers err
                      newFs = fs {fsErrors = [convertedErr]}
                  setFileState filePath newFs
                  sendFileState filePath newFs
                Right (typedExpr, state) -> do
                  sendEditTextsWithVersion filePath vfsVersion edits
                  logText "Refine: hole edit sent\n"
                  let lspMove = mkLSPMove (toLSPRange (holeRange hole)) finalImplText
                      movedFs = applyMovesToFileState [lspMove] fs
                      newSource = applyEdits source edits
                      (holes1, holes2) = splitAtFirst hole (fsHoles fs)
                      newHoles = justifyExpHoleRanges $ collectTypedHole typedExpr
                      holes2' = justifyRearHoleRanges (length newHoles - 1) (holeRange hole) holes2
                      newFs =
                        movedFs
                          { fsErrors = [],
                            fsHoles = updateHoleIds (holes1 <> newHoles <> holes2'),
                            fsTIState = state
                          }
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
-- innerEditsRel: hole-digging edits in (1,1)-based coords relative to implText;
-- used (combined with marker-removal edits) to convert post-edit error ranges back
-- to pre-edit source coordinates.
refineAndDig :: FilePath -> Int -> Spec -> Text -> Either Error (Text, [(Range, Text)], Either Error FileState)
refineAndDig filePath idCount spec source = do
  let specRng = specRange spec
  when (isSingleLine specRng) $
    Left (Others "Refine" "Spec should have more than one line." (Just specRng))
  let implRange = shrinkRange 2 specRng
      implText = extractText implRange source
  unless (isFirstLineBlank implText) $
    Left (Others "Refine" "The first line in the spec must be blank." (Just implRange))
  let implStart = rangeStart implRange

  (finalImplText, innerEditsRel, stmts) <- parseAndDigFragment filePath implStart implText
  return (finalImplText, innerEditsRel, loadConcreteFragment (specTypeEnv spec) idCount spec stmts)

refineHoleAndDig :: FilePath -> Hole -> Text -> Inference -> Either Error (Text, [(Range, Text)], Either Error (T.Expr, Inference))
refineHoleAndDig filePath hole source state = do
  let holeRng = holeRange hole
  unless (isSingleLine holeRng) $
    Left (Others "Refine" "Hole should have exact one line." (Just holeRng))
  let implRange = shrinkRange 2 holeRng
      implText = Text.strip $ extractText implRange source
      implStart = rangeStart implRange

  (finalImplText, innerEditsRel, expr) <- parseAndDigHoleFragment filePath implStart implText
  return (finalImplText, innerEditsRel, loadConcreteHoleFragment (holeTypeEnv hole) (holeType hole) expr state)

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
-- Returns (finalImplText, innerEditsRel, stmts), where innerEditsRel are the
-- hole-digging edits in (1,1)-based coordinates relative to implText.
parseAndDigFragment :: FilePath -> Pos -> Text -> Either Error (Text, [(Range, Text)], [C.Stmt])
parseAndDigFragment filePath fragmentStart implText = do
  -- TODO: reduce parsing
  (_, holes1) <- parseRelative implText
  let innerEditsRel = map (\(kind, range) -> (range, diggedText kind range)) holes1
  case holes1 of
    [] -> do
      stmts <- parseAbsolute implText
      Right (implText, [], stmts)
    _ -> do
      let newImplText = applyEdits implText innerEditsRel
      stmts2 <- parseAbsolute newImplText
      let holes2 = collectHole stmts2
      case holes2 of
        [] -> Right (newImplText, innerEditsRel, stmts2)
        _ -> Left (Others "Refine" "unexpected holes after digging" Nothing)
  where
    parseRelative src = do
      stmts <- parseFragmentStmts filePath (mkPos 1 1) src
      return (stmts, collectHole stmts)
    parseAbsolute = parseFragmentStmts filePath fragmentStart

parseAndDigHoleFragment :: FilePath -> Pos -> Text -> Either Error (Text, [(Range, Text)], C.Expr)
parseAndDigHoleFragment filePath fragmentStart implText = do
  (_, holes1) <- parseRelative implText
  let innerEditsRel = map (\(kind, range) -> (range, diggedText kind range)) holes1
  case holes1 of
    [] -> do
      expr <- parseAbsolute implText
      Right (implText, [], expr)
    _ -> do
      let newImplText = applyEdits implText innerEditsRel
      exprs2 <- parseAbsolute newImplText
      let holes2 = collectHole exprs2
      case holes2 of
        [] -> Right (newImplText, innerEditsRel, exprs2)
        _ -> Left (Others "Refine" "unexpected holes after digging" Nothing)
  where
    parseRelative src = do
      expr <- parseFragmentExpr filePath (mkPos 1 1) src
      return (expr, collectHole expr)
    parseAbsolute = parseFragmentExpr filePath fragmentStart

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
        (pos, specs, collectTypedHole impl, sws, counter')
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
-- Decompose a refine edit into fine-grained edits for error coordinate conversion

-- | Decompose the single "replace specRange with finalImplText" edit into:
--   [remove open marker] ++ inner hole-digging edits ++ [remove close marker].
--   Used for converting post-edit error ranges back to pre-edit coordinates.
--   The spec's open marker "[!" and close marker "!]" are each 2 chars.
--
--   Example. Given source:
--     1: var x : Int
--     2: [!
--     3:   x := 3 + ?
--     4: !]
--   with specRng = (2,1)-(4,3) and
--        innerEditsRel = [((2,12)-(2,13), "{! !}")]  -- relative to implText
--   this produces:
--     [ ((2,1)-(2,3),   "")        -- remove "[!"
--     , ((3,12)-(3,13), "{! !}")   -- ? translated to absolute coords
--     , ((4,1)-(4,3),   "")        -- remove "!]"
--     ]
--   Compared with the original single big edit over (2,1)-(4,3): if post-edit
--   analysis reports an error near "3 + {! !}", fine-grained edits let the
--   unchanged "3 + " part map back cleanly, whereas a single big edit would
--   expand the error to cover the whole spec.
decomposeSpecEdits :: Range -> [(Range, Text)] -> [(Range, Text)]
decomposeSpecEdits specRng innerEditsRel =
  let specStart = rangeStart specRng
      specEnd = rangeEnd specRng
      openMarkerEnd = mkPos (posLine specStart) (posCol specStart + 2)
      closeMarkerStart = mkPos (posLine specEnd) (posCol specEnd - 2)
      openRange = mkRange specStart openMarkerEnd
      closeRange = mkRange closeMarkerStart specEnd
      innerEditsAbs = map (\(r, t) -> (translateTokenRange openMarkerEnd r, t)) innerEditsRel
   in [(openRange, "")] ++ innerEditsAbs ++ [(closeRange, "")]

-- | Like decomposeSpecEdits but for a hole refine. Hole is single-line;
--   the open and close edits also absorb the leading/trailing whitespace that
--   is stripped before parsing. The hole's open marker "{!" and close marker "!}"
--   are each 2 chars.
--
--   The open edit uses a 2-char placeholder (not empty) as its replacement so
--   that the resulting post-edit column for stripped content matches what the
--   parser uses as its anchor: parseFragmentExpr is called with fragmentStart =
--   rangeStart implRange (= holeStart + 2 cols, right after "{!"), but the text
--   it parses is the stripped content. So in the parser's view, the stripped
--   content starts at holeStart + 2, regardless of leadingWs. The 2-char
--   placeholder makes our EditRegion.newEnd align with this.
--
--   Inner hole edits keep their origRange in original source coords (using
--   openOrigEnd = holeStart + 2 + leadingWs as the translation anchor), so
--   they correctly describe where the "?" actually lives in the source.
decomposeHoleEdits :: Range -> Text -> [(Range, Text)] -> [(Range, Text)]
decomposeHoleEdits holeRng origContent innerEditsRel =
  let holeStart = rangeStart holeRng
      holeEnd = rangeEnd holeRng
      leadingWs = Text.length (Text.takeWhile isSpace origContent)
      trailingWs = Text.length origContent - leadingWs - Text.length (Text.strip origContent)
      openOrigEnd = mkPos (posLine holeStart) (posCol holeStart + 2 + leadingWs)
      closeOrigStart = mkPos (posLine holeEnd) (posCol holeEnd - 2 - trailingWs)
      openRange = mkRange holeStart openOrigEnd
      closeRange = mkRange closeOrigStart holeEnd
      openPlaceholder = Text.replicate 2 " "
      innerEditsAbs = map (\(r, t) -> (translateTokenRange openOrigEnd r, t)) innerEditsRel
   in [(openRange, openPlaceholder)] ++ innerEditsAbs ++ [(closeRange, "")]

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
    { fsErrors = [], -- Refine success: clear previous errors
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
