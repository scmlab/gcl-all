{-# LANGUAGE OverloadedStrings #-}

module Server.Refine2 where

import Control.Monad (when)
import Data.Bifunctor (first, second)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Common (Index, TypeInfo)
import GCL.Predicate (Hole, InfMode (..), PO, Spec (..))
import GCL.Range (Pos (..), Range (..), extractText, mkPos, mkRange, posCol, rangeStart)
import GCL.Type (runElaboration)
import GCL.WP (collectStmtHoles, runWP, structStmts)
import GCL.WP.Types (StructError, StructWarning)
import Language.Lexer.Applicative (TokenStream (..))
import Server.Handler.GCL.Refine () -- for Elab [A.Stmt] orphan instance
import Server.Highlighting (collectHighlightingFromStmts)
import Server.Hover (collectHoverInfoFromStmts)
import Server.Load2 (DigResult, LoadResult (..), applyEdits, collectHolesFromStatements, diggedText)
import Server.Monad (HoleKind (..))
import qualified Language.LSP.Protocol.Types as LSP
import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import qualified Syntax.Concrete.Instances.ToAbstract as C
import Syntax.Parser.Error (ParseError (..))
import qualified Syntax.Parser as Parser
import Syntax.Parser.Lexer (TokStream, scan)
import GCL.Range (R (..))
import qualified Syntax.Typed as T

--------------------------------------------------------------------------------
-- Types are reused from Server.Load2 (LoadResult, DigResult)

--------------------------------------------------------------------------------
-- Main pipeline

-- | Full pipeline: validate spec → extract impl → parseAndDigFragment → loadConcreteFragment.
refineAndDig :: FilePath -> Int -> Spec -> Text
             -> Either Error (Maybe DigResult, LoadResult)
refineAndDig filePath idCount spec source = do
  let specRng = specRange spec
  when (isSingleLine specRng) $
    Left (Others "Refine" "Spec should have more than one line." (Just specRng))
  let implRange = shrinkRange 2 specRng
      implText  = extractText implRange source
  when (not (isFirstLineBlank implText)) $
    Left (Others "Refine" "The first line in the spec must be blank." (Just implRange))
  let implStart = rangeStart implRange
  (maybeDig, stmts) <- parseAndDigFragment filePath implStart implText
  result <- loadConcreteFragment (specTypeEnv spec) idCount spec stmts
  return (maybeDig, result)

-- | Pipeline from concrete stmts: abstract → elaborate → sweep.
loadConcreteFragment :: [(Index, TypeInfo)] -> Int -> Spec -> [C.Stmt]
               -> Either Error LoadResult
loadConcreteFragment typeEnv idCount spec stmts = do
  let abstract = C.runAbstractTransform stmts
  elaborated <- first TypeError $ runElaboration abstract typeEnv
  (pos, specs, holes, warnings, idCount') <-
    first StructError $ sweepFragment idCount spec elaborated
  return LoadResult
    { specifications   = specs
    , holes            = holes
    , proofObligations = pos
    , warnings         = warnings
    , idCount          = idCount'
    , semanticTokens   = collectHighlightingFromStmts stmts
    , definitionLinks  = mempty -- TODO: needs scope info from declarations
    , hoverInfos       = collectHoverInfoFromStmts elaborated
    }

-- | Parse fragment and dig holes if needed.
-- Internally uses relative positions (1,1) for hole digging,
-- then absolute positions (fragmentStart) for the returned stmts.
parseAndDigFragment :: FilePath -> Pos -> Text
                    -> Either Error (Maybe DigResult, [C.Stmt])
parseAndDigFragment filePath fragmentStart implText = do
  (_, holes1) <- parseRelative implText
  case holes1 of
    [] -> do
      stmts <- parseAbsolute implText
      Right (Nothing, stmts)
    _  -> do
      let (edits, newImplText) = replaceHoles implText holes1
      (stmts2, holes2) <- parseAbsolute' newImplText
      case holes2 of
        [] -> Right (Just (edits, newImplText), stmts2)
        _  -> Left (Others "Refine2" "unexpected holes after digging" Nothing)
  where
    parseRelative src = do
      stmts <- parseFragmentStmts filePath (mkPos 1 1) src
      return (stmts, collectHolesFromStatements stmts)
    parseAbsolute src =
      parseFragmentStmts filePath fragmentStart src
    parseAbsolute' src = do
      stmts <- parseAbsolute src
      return (stmts, collectHolesFromStatements stmts)
    replaceHoles src holeList = (edits, applyEdits src edits)
      where
        edits = map (\(kind, range) -> (range, diggedText kind range)) holeList

--------------------------------------------------------------------------------
-- Fragment parsing

-- | Parse text as a list of statements with position translation.
parseFragmentStmts :: FilePath -> Pos -> Text -> Either Error [C.Stmt]
parseFragmentStmts filePath fragmentStart src =
  let tokens  = scan filePath src
      tokens' = translateTokStream fragmentStart tokens
   in case Parser.parse Parser.statements filePath tokens' of
        Left (errors, logMsg) -> Left (ParseError (SyntacticError errors logMsg))
        Right stmts           -> Right stmts

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
  mkPos (lineStart + lineOff - 1)
        (if lineOff == 1 then colStart + colOff - 1 else colOff)

--------------------------------------------------------------------------------
-- Sweep fragment

-- | Sweep a fragment using the spec's pre/post conditions.
sweepFragment :: Int -> Spec -> [T.Stmt]
              -> Either StructError ([PO], [Spec], [Hole], [StructWarning], Int)
sweepFragment counter (Specification _ pre post _ _) impl =
  second
    (\(_, counter', (pos, specs, sws, _redexes)) ->
        (pos, specs, concatMap collectStmtHoles impl, sws, counter'))
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
