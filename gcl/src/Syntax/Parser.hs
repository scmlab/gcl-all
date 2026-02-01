{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser
  ( Parser(..),
    scanAndParse,
    parse,
    parseWithTokList,
    program,
    statements,
    statement,
    expression
  )
where

import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GCL.Range
import Language.Lexer.Applicative (TokenStream (TsEof, TsToken))
import Syntax.Concrete hiding (Op)
import Syntax.Parser.Types
import Syntax.Parser.Error
import Syntax.Parser.Lexer
import Syntax.Parser.Expr (expression)
import qualified Syntax.Parser.Stmt as Stmt
import Syntax.Parser.Program (program)
import Text.Megaparsec hiding
  ( ParseError,
    Pos,
    State,
    Token,
    parse,
    tokens,
  )
import qualified Text.Megaparsec as Mega
import Prelude hiding
  ( EQ,
    GT,
    LT,
    Ordering,
    lookup,
  )

scanAndParse :: Parser a -> FilePath -> Text -> Either ParseError a
scanAndParse parser filepath source =
  let tokens = scan filepath source
   in case parse parser filepath tokens of
        Left (errors, logMsg) -> throwError (SyntacticError errors logMsg)
        Right val -> return val

parse :: Parser a -> FilePath -> TokStream -> Either (NonEmpty (Maybe Range, String), String) a
parse parser filepath tokenStream =
  case runM (runParserT (parser <* eof) filepath tokenStream) of
    (Left e, logMsg) -> Left (fromParseErrorBundle e, logMsg)
    (Right x, _) -> Right x
  where
    fromParseErrorBundle ::
      (ShowErrorComponent e) =>
      ParseErrorBundle TokStream e ->
      NonEmpty (Maybe Range, String)
    fromParseErrorBundle (ParseErrorBundle errors _) = fmap toError errors
      where
        toError ::
          (ShowErrorComponent e) => Mega.ParseError TokStream e -> (Maybe Range, String)
        toError err = (getRange' err, parseErrorTextPretty err)
        -- get the Range of all unexpected tokens
        getRange' :: (ShowErrorComponent e) => Mega.ParseError TokStream e -> Maybe Range
        getRange' (TrivialError _ (Just (Tokens xs)) _) = foldMap (Just . rangeOf) xs
        getRange' _ = Nothing

parseWithTokList ::
  Parser a -> FilePath -> [R Tok] -> Either (NonEmpty (Maybe Range, String), String) a
parseWithTokList parser filepath toks =
  parse parser filepath (convert toks)
  where
    convert :: [R Tok] -> TokStream
    convert (x : xs) = TsToken x (convert xs)
    convert [] = TsEof

statements :: Parser [Stmt]
statements = Stmt.statements program

statement :: Parser Stmt
statement = Stmt.statement program
