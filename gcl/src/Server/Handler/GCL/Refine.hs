{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Server.Handler.GCL.Refine where

import Control.Monad.Except (runExcept)
import qualified Data.Aeson as JSON
import Data.Bifunctor (bimap)
import Data.List (find)
import Data.Loc.Range (Pos (..), R (..), Range (..), mkRange, rangeStart, rangeEnd)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (Others, ParseError, StructError, TypeError))
import GCL.Common (Index, TypeInfo)
import GCL.Predicate (InfMode (..), PO (..), Spec (..))
import GCL.Type (Elab (..), TypeError, Typed, runElaboration)
import GCL.WP
import GCL.WP.Types (StructError, StructWarning (..))
import GHC.Generics (Generic)
import Language.Lexer.Applicative (TokenStream (..))
import Pretty (pretty)
import Server.Monad (FileState (..), ServerM, deleteSpec, editTexts, loadFileState, logText, logTextLn, pushPos, pushSpecs, pushWarnings, readSource, updateIdCounter)
import Server.Notification.Error (sendErrorNotification)
import Server.Notification.Update (sendUpdateNotification)
import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import qualified Syntax.Parser as Parser
import Syntax.Parser.Error (ParseError (..))
import Syntax.Parser.Lexer (TokStream, scan)
import qualified Syntax.Typed as T

data RefineParams = RefineParams
  { filePath :: FilePath,
    line :: Int, -- 0-based
    character :: Int -- 0-based
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON RefineParams

instance JSON.ToJSON RefineParams

handler :: RefineParams -> (() -> ServerM ()) -> (() -> ServerM ()) -> ServerM ()
handler _params@RefineParams {filePath, line, character} onFinish _ = do
  -- convert from 0-based to 1-based, and we do not care about the offset here
  let cursor = Pos filePath (line + 1) (character + 1) (-1)
  logTextLn $ Text.pack $ "refine: cursor: " ++ show cursor
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> do
      onError (Others "Refine Error" "File not loaded in server." Nothing)
    Just fileState -> do
      logTextLn $ Text.pack $ "got fileState. specfications: " ++ show (specifications fileState)
      case findEnclosingSpec cursor (map snd $ specifications fileState) of
        Nothing -> do
          logTextLn $ Text.pack $ "cursor: " ++ show cursor
          logTextLn $ Text.pack $ "all the spec ranges: " ++ show (map (specRange . snd) $ specifications fileState)
          onError (Others "Refine Error" "No enclosing spec found." Nothing) -- TODO: show line column ?

        -- spec
        Just spec -> do
          logTextLn $ Text.pack $ "spec found: " ++ show spec
          if isSingleLine $ specRange spec
            then do
              onError (Others "Refine Error" "Spec should have more than one line." (Just $ specRange spec))
            else do
              -- implRange
              let implRange = shrinkRange 2 (specRange spec) -- excludes [! and !]
              logTextLn $ Text.pack $ "spec implRange: " ++ show implRange
              maybeSource <- readSource filePath
              case maybeSource of
                Nothing -> do
                  onError (Others "Refine Error" ("Source not found: filePath: " ++ filePath) Nothing)
                Just source -> do
                  logTextLn $ Text.pack "source: " <> source
                  -- implText
                  let implText = getRangeText implRange source
                  logText "==== implText ====####"
                  logText implText
                  logTextLn "####===="
                  if not (isFirstLineBlank implText)
                    then do
                      onError (Others "Refine Error" "The first line in the spec must be blank." (Just implRange))
                    else do
                      -- implStart
                      let implStart = rangeStart implRange
                      logTextLn $ Text.pack $ "implStart: " ++ show implStart
                      -- digImplHoles will replace all the question marks "?" to brackets "[! !]" with proper indent
                      case digImplHoles implStart filePath implText of
                        Left err -> do
                          onError (ParseError err)
                        Right holelessImplText -> do
                          logText "==== holelessImplText ====####"
                          logText holelessImplText
                          logTextLn "####===="
                          -- text to concrete
                          -- (use specStart as the starting position in parse/toAbstract/elaborate)
                          logText "  parsing\n"
                          logText "    implStart =\n"
                          logText . Text.pack . show . pretty $ implStart
                          logText "\n"
                          case parseFragment implStart holelessImplText of
                            Left err -> onError (ParseError err)
                            Right concreteImpl -> do
                              -- concrete to abstract
                              logText "  text parsed\n"
                              case toAbstractFragment concreteImpl of
                                Nothing -> do
                                  error "Holes still found after digging all holes. Should not happen\n"
                                Just abstractImpl -> do
                                  logText "  abstracted\n"
                                  -- elaborate
                                  let typeEnv = specTypeEnv spec
                                  logText " type env:\n"
                                  logText (Text.pack $ show typeEnv)
                                  logText "\n"
                                  -- TODO:
                                  -- 1. Load: 在 elaborate program 的時候，要把 specTypeEnv 加到 spec 裡 (Andy) ok
                                  -- 2. Load: 在 sweep 的時候，改成輸入 elaborated program，把 elaborated program 裡面的 spec 的 typeEnv 加到輸出的 [Spec] 裡 (SCM)
                                  -- 3. Refine: elaborateFragment 裡面要正確使用 typeEnv (Andy) ok
                                  case elaborateFragment typeEnv abstractImpl of
                                    Left err -> do
                                      logText "  type error\n"
                                      onError (TypeError err)
                                    Right typedImpl -> do
                                      -- get POs and specs
                                      logText "  type checked\n"
                                      let FileState {idCount} = fileState
                                      case sweepFragment idCount spec typedImpl of
                                        Left err -> onError (StructError err)
                                        Right (innerPos, innerSpecs, innerWarnings, idCount') -> do
                                          logText "  swept\n"
                                          logTextLn . Text.pack $ "==== refine: innerSpecs:" ++ show innerSpecs
                                          logTextLn "\n===="
                                          --
                                          -- Now we have all the data.
                                          -- We are going to modify the state and the source code.
                                          --
                                          deleteSpec filePath spec -- delete outer spec (by id)
                                          logText "  outer spec deleted (refine)\n"
                                          let FileState {editedVersion} = fileState
                                          updateIdCounter filePath idCount'
                                          logText "  counter updated (refine)\n"
                                          --
                                          -- During parsing we assigned positions to the innerXXX items.
                                          -- Afterwards we modify the source (we remove the outer markers "[!" and "!]").
                                          -- Do we need to adjust the positions of those innerXXX entries?
                                          --
                                          -- Currently no.
                                          --
                                          -- Because we only remove the outer markers "[!" and "!]" and
                                          -- there should be nothing after either "[!" or "!]" on the same line,
                                          -- the previously assigned positions remain valid and do not need adjustment.
                                          --
                                          pushSpecs (editedVersion + 1) filePath innerSpecs
                                          pushPos (editedVersion + 1) filePath innerPos
                                          pushWarnings (editedVersion + 1) filePath innerWarnings
                                          logText "  new specs and POs added (refine)\n"

                                          -- send notification to update Specs and POs
                                          logText "refine: success\n"
                                          sendUpdateNotification filePath
                                          -- clear errors
                                          sendErrorNotification filePath []
                                          logText "refine: update notification sent\n"
                                          -- edit source (dig holes + remove outer brackets)
                                          editTexts filePath [(specRange spec, holelessImplText)] do
                                            logText "  text edited (refine)\n"
                                            onFinish ()
  where
    onError :: Error -> ServerM ()
    onError err = do
      logText "refine: error\n\t"
      logText $ Text.pack (show $ pretty err)
      logText "\n"
      sendErrorNotification filePath [err]
      logText "refine: update notification sent\n"
      onFinish () -- TODO: reconsider it

    --
    --  [ ! ..... ! ]
    -- N Y Y ... Y Y N
    --
    findEnclosingSpec :: Pos -> [Spec] -> Maybe Spec
    findEnclosingSpec pos specs = find (\spec -> isInRange pos $ shrinkRange 1 $ specRange spec) specs
      where
        isInRange pos (Range p1 p2) = p1 `posLE` pos && pos `posLE` p2
        posLE (Pos _ l1 c1 _) (Pos _ l2 c2 _) = (l1, c1) <= (l2, c2) -- ignore files and offsets (unlike "compareWithPosition")

    -- l1 l2 c1 c2 are all 1-based
    -- l1 / l2 / c1 are inclusive, but c2 is exclusive
    -- TODO: performance / rangeLinesFromVfs
    getRangeText :: Range -> Text -> Text
    getRangeText (Range (Pos _f1 l1 c1 _o1) (Pos _f2 l2 c2 _o2)) text = result
      where
        rangeLines = drop (l1 - 1) $ take l2 $ Text.lines text -- split by '\n', but may contain '\r'
        resultLines = modifyFirst (Text.drop $ c1 - 1) $ modifyLast (Text.take $ c2 - 1) rangeLines
        result = Text.intercalate "\n" resultLines

        -- or use Lens
        modifyFirst :: (a -> a) -> [a] -> [a]
        modifyFirst f (x : xs) = f x : xs
        modifyFirst f [] = error "modifyFirst: empty list"

        modifyLast :: (a -> a) -> [a] -> [a]
        modifyLast f (x : []) = [f x]
        modifyLast f (x : xs) = x : modifyLast f xs
        modifyLast f [] = error "modifyLast: empty list"

    isSingleLine :: Range -> Bool
    isSingleLine (Range (Pos _f1 l1 _c1 _o1) (Pos _f2 l2 _c2 _o2)) = l1 == l2

    shrinkRange :: Int -> Range -> Range
    shrinkRange diff (Range (Pos f1 l1 c1 o1) (Pos f2 l2 c2 o2)) =
      mkRange (Pos f1 l1 (c1 + diff) (o1 + diff)) (Pos f2 l2 (c2 - diff) (o2 - diff))

    isFirstLineBlank :: Text -> Bool
    isFirstLineBlank = Text.null . Text.strip . Text.takeWhile (/= '\n')

collectFragmentHoles :: [C.Stmt] -> [Range]
collectFragmentHoles = concat . map collectStmt
  where
    collectStmt (C.SpecQM range) = [range]
    collectStmt (C.Do _ ss _) = collectSepBy ss
    collectStmt (C.If _ ss _) = collectSepBy ss
    collectStmt _ = []

    collectSepBy (C.Head gcmd) = collectGdCmd gcmd
    collectSepBy (C.Delim gcmd _ ss) = collectGdCmd gcmd ++ collectSepBy ss

    collectGdCmd (C.GdCmd _ _ stmts) = concat (map collectStmt stmts)

digImplHoles :: Pos -> FilePath -> Text -> Either ParseError Text
digImplHoles parseStart filePath implText =
  -- use `parseStart` for absolute positions in error messages
  case parseFragment parseStart implText of
    Left err -> Left err
    Right _ ->
      -- use `Pos filePath 1 1 0` for relative position of the hole reported
      case parseFragment (Pos filePath 1 1 0) implText of
        Left _ -> error "should not happen"
        Right concreteImpl ->
          case collectFragmentHoles concreteImpl of
            [] -> return implText
            Range start _ : _ -> digImplHoles parseStart filePath $ digFragementHole start implText
  where
    digFragementHole :: Pos -> Text -> Text
    digFragementHole (Pos _path lineNumber col _charOff) fullText =
      Text.intercalate "\n" linesEdited
      where
        allLines :: [Text]
        allLines = Text.lines fullText -- split fullText by '\n'
        lineToEdit :: Text
        lineToEdit = allLines !! (lineNumber - 1)
        beforeHole = Text.take (col - 1) lineToEdit
        afterHole = Text.drop col lineToEdit -- lineToEdit
        indentation n = Text.replicate n " "
        lineEdited :: Text
        lineEdited =
          beforeHole
            <> "[!\n"
            <> indentation (col - 1)
            <> "\n"
            <> indentation (col - 1)
            <> "!]"
            <> afterHole
        linesEdited :: [Text]
        linesEdited = take (lineNumber - 1) allLines ++ [lineEdited] ++ drop lineNumber allLines

-- `fragmentStart :: Pos` is used to translate the locations in the parse result
parseFragment :: Pos -> Text -> Either ParseError [C.Stmt]
parseFragment fragmentStart fragment = do
  let Pos filePath _ _ _ = fragmentStart
  let tokens = Syntax.Parser.Lexer.scan filePath fragment
  let tokens' = translateTokStream fragmentStart tokens
  case Parser.parse Parser.statements filePath tokens' of
    Left (errors, logMsg) -> Left (SyntacticError errors logMsg)
    Right val -> Right val
  where
    translateRange :: Pos -> Pos -> Pos
    translateRange
      _fragmentStart@(Pos _ lineStart colStart coStart)
      (Pos path lineOffset colOffset coOffset) =
        Pos path line col co
        where
          line = lineStart + lineOffset - 1
          col =
            if lineOffset == 1
              then colStart + colOffset - 1
              else colOffset
          co = coStart + coOffset

    translateTokenRange :: Pos -> Range -> Range
    translateTokenRange fragmentStart (Range left right) =
      mkRange (translateRange fragmentStart left) (translateRange fragmentStart right)

    translateTokStream :: Pos -> Syntax.Parser.Lexer.TokStream -> Syntax.Parser.Lexer.TokStream
    translateTokStream fragmentStart (TsToken (R range x) rest) =
      TsToken (R (translateTokenRange fragmentStart range) x) (translateTokStream fragmentStart rest)
    translateTokStream _ TsEof = TsEof
    translateTokStream _ (TsError e) = TsError e

toAbstractFragment :: [C.Stmt] -> Maybe [A.Stmt]
toAbstractFragment concreteFragment =
  case runExcept $ C.toAbstract concreteFragment of
    Left _ -> Nothing
    Right abstractFragment -> Just abstractFragment

elaborateFragment :: (Elab a) => [(Index, TypeInfo)] -> a -> Either TypeError (Typed a)
elaborateFragment typeEnv abstractFragment = do
  runElaboration abstractFragment typeEnv

instance Elab [A.Stmt] where
  -- elaborate :: a -> TypeEnv -> ElaboratorM (Maybe Type, Typed a, Subs Type)
  elaborate stmts env = do
    typed <-
      mapM
        ( \stmt -> do
            (_, typed, _) <- elaborate stmt env
            return typed
        )
        stmts
    return (Nothing, typed, mempty)

sweepFragment :: Int -> Spec -> [T.Stmt] -> Either StructError ([PO], [Spec], [StructWarning], Int)
sweepFragment counter (Specification _ pre post _ _) impl =
  bimap
    id
    ( \(_, counter', (pos, specs, sws, _)) ->
        (pos, specs, sws, counter')
    )
    $ runWP
      (structStmts Primary (pre, Nothing) impl post)
      (Map.empty, []) -- SCM: this can't be right.
      counter
