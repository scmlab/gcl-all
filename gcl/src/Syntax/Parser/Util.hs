{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Syntax.Parser.Util
  ( M,
    Parser,
    runM,
    getLastToken,
    getLoc,
    withLoc,
    getRange,
    withRange,
    getMaybeRange,
    withMaybeRange,
    logIfSuccess,
    withLog,
    clampLog,
    plog,
    symbol,
    extract,
    ignore,
    ignoreP,
    sepByAlignmentOrSemi,
    sepByAlignmentOrSemi1,
    sepByAlignment,
    sepByAlignment1,
  )
where

import Control.Monad (guard, (>=>))
import Control.Monad.State
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NEL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Endo (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void
import GCL.Range
import Syntax.Parser.Lexer
  ( Tok (..),
    TokStream,
  )
import Text.Megaparsec hiding
  ( Pos,
    State,
    between,
  )

--------------------------------------------------------------------------------

-- | Source location bookkeeping
type M = StateT Bookkeeping (Writer (Endo [String]))

-- About the Endo stuff, the reference: https://stackoverflow.com/questions/53785921/how-efficient-is-the-writer-monad-for-lists
--  and see the "## Logging" section in this file.

type ID = Int

data Bookkeeping = Bookkeeping
  { currentRange :: Maybe Range, -- current Range mark (Nothing = no range yet)
    lastToken :: Maybe Tok, -- the last accepcted token
    opened :: Set ID, -- waiting to be moved to the "logged" map
    -- when the starting position of the next token is determined
    logged :: Map ID (Maybe Range), -- waiting to be removed when the ending position is determined
    index :: Int, -- for generating fresh IDs
    indentStack :: [R Tok] -- Recording the tokens to indent/align to.
    -- see the section: "## State (Bookkeeping) actions for indentation"
  }

runM :: StateT Bookkeeping (Writer (Endo [String])) a -> (a, String)
runM f =
  let (a, pl) = runWriter $ evalStateT f (Bookkeeping Nothing Nothing Set.empty Map.empty 0 [])
   in (a, "parsing log:\n" <> intercalate "\n" (appEndo pl []))

getCurrentRange :: M (Maybe Range)
getCurrentRange = gets currentRange

getLastToken :: M (Maybe Tok)
getLastToken = gets lastToken

-- | Returns an ID (marking the start of the range of some source location)
markStart :: M ID
markStart = do
  i <- gets index
  modify $ \st -> st {index = succ i, opened = Set.insert i (opened st)}
  return i

-- | Returns the range of some source location.
--   The range starts from where the ID is retreived, and ends from here
markEnd :: ID -> M (Maybe Range)
markEnd i = do
  end <- getCurrentRange
  loggedPos <- gets logged
  let loc = case Map.lookup i loggedPos of
        Nothing -> Nothing
        Just start -> start <---> end
  modify $ \st -> st {logged = Map.delete i loggedPos}
  return loc

-- | Updates the current source range
updateRange :: Range -> M ()
updateRange range = do
  set <- gets opened
  let addedLoc = Map.fromSet (const (Just range)) set
  modify $ \st ->
    st
      { currentRange = Just range,
        opened = Set.empty,
        logged = Map.union (logged st) addedLoc
      }

-- | Updates the latest scanned token
updateToken :: Tok -> M ()
updateToken tok = modify $ \st -> st {lastToken = Just tok}

--------------------------------------------------------------------------------

-- | Helper functions
type Parser = ParsecT Void TokStream M

getLoc :: Parser a -> Parser (a, Maybe Range)
getLoc parser = do
  i <- lift markStart
  result <- parser
  maybeRange <- lift (markEnd i)
  return (result, maybeRange)

getRange :: Parser a -> Parser (a, Range)
getRange parser = do
  (result, maybeRange) <- getLoc parser
  case maybeRange of
    Nothing -> error "NoLoc when getting srcloc info from a token"
    Just range -> return (result, range)

withLoc :: Parser (Maybe Range -> a) -> Parser a
withLoc parser = do
  (result, maybeRange) <- getLoc parser
  return $ result maybeRange

withRange :: Parser (Range -> a) -> Parser a
withRange parser = do
  (result, range) <- getRange parser
  return $ result range

-- | Like getLoc but returns Maybe Range (Nothing for NoLoc)
getMaybeRange :: Parser a -> Parser (a, Maybe Range)
getMaybeRange = getLoc -- Now getLoc already returns Maybe Range

-- | Like withLoc but uses Maybe Range
withMaybeRange :: Parser (Maybe Range -> a) -> Parser a
withMaybeRange = withLoc -- Now withLoc already uses Maybe Range

--------------------------------------------------------------------------------
-- ## Logging

-- | Log after the parser successes.
logIfSuccess :: (a -> String) -> Parser a -> Parser a
logIfSuccess msgF p = do
  x <- p
  lift $ tell $ Endo ([msgF x] ++)
  return x

-- | Log before running the parser.
withLog :: String -> Parser a -> Parser a
withLog msg p = lift (tell $ Endo ([msg] ++)) >> p

clampLog :: String -> (a -> String) -> Parser a -> Parser a
clampLog before after = logIfSuccess after . withLog before

-- for debugging, a simple log
plog :: String -> Parser ()
plog msg = lift $ tell $ Endo ([msg] ++)

--------------------------------------------------------------------------------
-- ## State (Bookkeeping) actions for indentation.

-- See the section "## Combinators for indentation" below for explanations
-- of the concept of indentation parsing.

-- About the state actions for indentation:
-- An indentation requirement--a token--is inserted to and popped from the stack 'indentStack'
-- only by the combinator 'indentTo'.
-- When any indentation requirement being on top of the stack, the atomic token extractors:
-- 'symbol' and 'extract', of which all parsers are based upon, would check if the incoming
-- token is indent to that requirement (through 'extractWithIndentCheck' function).

insertIndentReq :: R Tok -> Parser ()
insertIndentReq tok = do
  stack <- gets indentStack
  modify $ \st -> st {indentStack = tok : stack}

popIndentReq :: Parser (Maybe (R Tok))
popIndentReq = do
  stack <- gets indentStack
  case stack of
    [] -> return Nothing
    p : ps -> do
      modify $ \st -> st {indentStack = ps}
      return (Just p)

lastIndentReq :: Parser (Maybe (R Tok))
lastIndentReq = do
  stack <- gets indentStack
  case stack of
    [] -> return Nothing
    tok : _ -> return (Just tok)

colOf :: R Tok -> Int
colOf = posCol . rangeStart . rangeOf

fitsIndentReq :: R Tok -> Maybe (R Tok) -> Bool
fitsIndentReq tokToCheck indentReq = case indentReq of
  Nothing -> True
  Just tokToAlign ->
    (colOf tokToCheck > colOf tokToAlign)
      || (tokToCheck `strictEq` tokToAlign)
      -- If the token is the same to the leftTip(the token to align/indent to).
      -- This could happen when backtracking happens;
      -- For example, in 'definition = choice [try funcDefnSig, typeDefn, funcDefnF]',
      -- the starts of both funcDefnSig and funcDefnF are identifiers, when funcDefnSig fails then goes to funcDefnF,
      -- the starting identifier would be checked again.
      || ( unR tokToCheck
             `elem` [ TokFi, -- 'fi'
                      TokOd, -- 'od'
                      TokBlockClose, -- ']|'
                      TokGuardBar, -- '|'
                      TokSpecClose, -- '!]'
                      -- , TokProofClose  -- '-}'
                      TokDeclClose -- ':}'
                    ]
         )
  where
    -- Special cases: structural delimiters don't need to align/indent to anything
    -- or if they're not aligned, will it cause any trouble?
    -- Array's ']', quant's '|>' are considered parts of an expression, therefore not on the list.

    strictEq (R r1 t1) (R r2 t2) = r1 == r2 && t1 == t2

-- an ideal method which doesn't work:

-- * parse -> if success, indentCheck(without touching parser state), manually fail it when indentCheck fails

-- So the current solution is:

-- * parse(Mega.token) with indentation check, if failed, see if the failure was caused by indentation error,

--   if so, add the indentation error message.
extractWithIndentCheck :: (R Tok -> Maybe (R Tok, a)) -> Parser a
extractWithIndentCheck tokpred = do
  ir <- lastIndentReq
  let f (rt, a) = do
        guard $ rt `fitsIndentReq` ir
        return a
  pr <- observing $ lookAhead anySingle -- later indent check needs a token
  case pr of
    Left _ -> do
      -- safe to assume that indentation won't be involved since there's no next token
      (_, a) <- token tokpred Set.empty
      return a
    Right rtok -> do
      -- now do the real extraction we need
      r <- observing $ token (tokpred >=> f) Set.empty
      -- delay the failure so we can add error messages for indentation
      case r of
        Right a -> return a -- successfully extract a will-indented token
        Left pe -> case pe of -- the extraction failed
          TrivialError _ m_ei set -> do
            if rtok `fitsIndentReq` ir
              then -- the failure was not caused by indentation error
                failure m_ei set
              else -- the failure was caused by indentation error, we need to proceed to adding error msg
                case m_ei of -- trying to utilize the original unexpected token's loc information
                  Nothing -> failureWithoutLoc
                  Just ei -> case ei of
                    Tokens ((R range errtok) NEL.:| tos) ->
                      failure (Just $ Tokens (newErrRTok range errtok NEL.:| tos)) set
                    Label _ -> failureWithoutLoc -- might need to change in the future
                    EndOfInput -> failureWithoutLoc -- a case that might not going to happen, for we filtered out the case at 'observing $ lookAhead anySingle'
            where
              fromJust Nothing = error "An error that shouldn't happen here: fitsIndentReq==False implies that 'ir' is a Just."
              fromJust (Just x) = x
              irtok = fromJust ir
              lineNum = posLine $ rangeStart $ rangeOf irtok
              newMsg = NEL.fromList $ "token '" <> show rtok <> "' not indent to '" <> show irtok <> "' of line " <> show lineNum
              newErrRTok range errtok = R range (ErrTokIndent errtok (unR irtok) lineNum)
              failureWithoutLoc = failure (Just $ Label newMsg) set
          FancyError _ set -> fancyFailure set -- We're not handling fancy errors yet.

--------------------------------------------------------------------------------

-- | Combinators

-- Parsing with bookkeeping actions: symbol, extract
-- All parsers should be built upon these combinators.

-- Create a parser of some symbol (while respecting source locations)
symbol :: Tok -> Parser Range
symbol t = do
  -- ir <- lastIndentReq
  -- loctok@(R range tok) <- satisfy (\rt@(R _ t') -> t == t' && rt `fitsIndentReq` ir)
  (range, tok) <- extractWithIndentCheck (\rt@(R r t') -> if t == t' then Just (rt, (r, t')) else Nothing)
  lift $ do
    updateRange range
    updateToken tok
  return range

-- Useful for extracting values from a Token
extract :: (Tok -> Maybe a) -> Parser a
extract f = do
  -- ir <- lastIndentReq
  let p rtok@(R r tok') = do
        -- guard $ rtok `fitsIndentReq` ir
        (\result -> (rtok, (result, r, tok'))) <$> f tok'
  -- (result, rtok@(R range tok)) <- token p Set.empty
  (result, range, tok) <- extractWithIndentCheck p
  lift $ do
    updateRange range
    updateToken tok
  return result

-- Create a parser of some symbol, that doesn't update source locations
-- effectively excluding it from source location tracking
ignore :: Tok -> Parser ()
ignore t = do
  R _ tok <- satisfy ((==) t . unR)
  lift $ updateToken tok
  return ()

-- The predicate version of `ignore`
ignoreP :: (Tok -> Bool) -> Parser ()
ignoreP p = do
  R _ tok <- satisfy (p . unR)
  lift $ updateToken tok
  return ()

-- ## Combinators for indentation
-- The design principle of indentation constraints is to reduce ambiguity of newline and alignment.
-- (if the new line is indented, then it's the continuation of the last line; if it's aligned,
--  then it's a parallel item.)
-- This idea is fulfilled by the combinator(s) 'sepByAlignment', which tries to run the input parser
-- as many as possible, while each result is aligned one by one, and the body of each is indented to the aligned column.

-- The input Parser should be built from either 'symbol' or 'extract', for our parsers are all built on these functions,
-- and each checks the indentation correctness of the token to be consumed.

----------------------------------------------------------------------------------------
-- ### Combinators for current internal use: indentTo, alignmentCheck

indentTo :: Parser a -> R Tok -> Parser a
indentTo p tok = do
  insertIndentReq tok
  x <- observing p
  _ <- popIndentReq
  -- The requirement needs to be popped no matter the parser went successfully or not,
  -- because Bookkeeping is not back-trackable (cannot just undo insertion of IndentReq).
  case x of
    Left pe -> do
      case pe of
        TrivialError _ m_ei set -> failure m_ei (Set.insert (Label (NEL.fromList $ "token indent to '" <> show tok <> "' of line " <> show lineNum)) set)
          where
            -- The message here is "expecting indented token", while in 'extractWithIndentCheck', it's "unexpected unindented token".
            -- Is this message really needed?

            lineNum = posLine $ rangeStart $ rangeOf tok
        FancyError _ set -> fancyFailure set
    Right r -> return r

alignmentCheck :: R Tok -> R Tok -> Parser ()
alignmentCheck rtok tokToAlign = do
  let lineNum = posLine $ rangeStart $ rangeOf tokToAlign
  if colOf rtok == colOf tokToAlign
    then return ()
    else failure Nothing (Set.fromList [Label (NEL.fromList $ "token align to '" <> show tokToAlign <> "' of line " <> show lineNum)])

-- A previous method, might be ok to export?
-- -- Do not use it like: "alignTo (p `indentTo` tok) tokToAlign", the wrong order of alignTo and indentTo would cause error,
-- -- use "alignmentCheck >> (p `indentTo` tok)" instead.
-- alignTo :: Parser a -> L Tok -> Parser a
-- alignTo parser tokToAlign = do
--   r <- observing $ lookAhead anySingle
--   case r of
--     Left _ -> parser
--       -- Let 'parser' decide if eof should fail or not, and the failure message.
--       -- This implies eof satisfies any indentation requirements.
--     Right tok -> do
--       alignmentCheck tok tokToAlign
--       parser
--  -- let notEof = do
--   --       tok <- lookAhead anySingle
--   --       alignmentCheck tok tokToAlign
--   --       p
--   -- notEof <|>  p
--   -- The method above doesn't work, because alignmentCheck doesn't change the parser state (Mega.State,
--   -- not our bookkeeping state), so the failure of alignment would just let it fails 'notEof' and then
--   -- tries p, instead of returning the failure.

----------------------------------------------------------------------------------------
-- Helpers for efficient list append

type List' a = Endo [a]

emptyList' :: List' a
emptyList' = Endo ([] ++)

list' :: [a] -> List' a
list' xs = Endo (xs ++)

toList :: List' a -> [a]
toList x = appEndo x []

----------------------------------------------------------------------------------------
-- ### Exported alignment combinators

sepByAlignmentOrSemi :: Parser a -> Parser [a]
sepByAlignmentOrSemi = sepByAlignmentOrSemiHelper True False

-- "True False" means "useSemi","not atLeastOne"

sepByAlignmentOrSemi1 :: Parser a -> Parser [a]
sepByAlignmentOrSemi1 = sepByAlignmentOrSemiHelper True True

-- "True True" means "useSemi","atLeastOne"

-- -- Tried an more general version
sepByAlignmentOrSemiHelper :: Bool -> Bool -> Parser a -> Parser [a]
sepByAlignmentOrSemiHelper useSemi atLeastOne' parser' = do
  toList <$> recursion Nothing atLeastOne' parser' emptyList'
  where
    recursion :: Maybe (R Tok) -> Bool -> Parser a -> List' a -> Parser (List' a)
    recursion m_tokToAlign atLeastOne parser accResult = do
      a <- observing $ lookAhead anySingle
      -- see if there exists a next token to check alignment
      case a of
        Left _ ->
          if atLeastOne
            then (accResult <>) . list' <$> some parser -- Let 'some parser' decide if eof should fail or not, and the failure message.
            else (accResult <>) . list' <$> many parser -- Let 'many parser' decide if eof should fail or not, and the failure message.
            -- These cases above imply that eof satisfies any indentation/alignment requirement.
        Right leadTok -> do
          let alignAndIndentBody = do
                mapM_ (alignmentCheck leadTok) m_tokToAlign -- Check alignment if there's a token to align to.
                parser `indentTo` leadTok
          let oneLeadBySemi = symbol TokSemi *> parser `indentTo` leadTok
          let semiParser = if useSemi then oneLeadBySemi else empty
          let onePass = (:) <$> alignAndIndentBody <*> many semiParser
          parseOnce <-
            if atLeastOne
              then Just <$> onePass -- If onePass doesn't success, parsing failure happens, because at least one result is needed.
              else optional onePass
          case parseOnce of
            Nothing -> return accResult
            Just rs -> recursion (Just leadTok) False parser (accResult <> list' rs)

-- Let the leadTok be the tokToAlign for the next parallel item.

sepByAlignment :: Parser a -> Parser [a]
sepByAlignment = sepByAlignmentOrSemiHelper False False

-- "False False" means "not useSemi","not atLeastOne"

sepByAlignment1 :: Parser a -> Parser [a]
sepByAlignment1 = sepByAlignmentOrSemiHelper False True

-- "False True" means "not useSemi","atLeastOne"
