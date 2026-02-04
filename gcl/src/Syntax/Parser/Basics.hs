{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Basics where

import Data.Text (Text)
import Syntax.Common hiding (Fixity (..))
import Syntax.Concrete hiding (Op)
import Syntax.Parser.Lexer
import Syntax.Parser.Token (tokenComma)
import Syntax.Parser.Types
import Syntax.Parser.Util
import Text.Megaparsec hiding
  ( ParseError,
    Pos,
    State,
    Token,
    parse,
    tokens,
  )
import Prelude hiding
  ( EQ,
    GT,
    LT,
    Ordering,
    lookup,
  )

-- TODO: LitChar
literal :: Parser Lit
literal =
  withRange
    ( choice
        [ LitBool True <$ symbol TokTrue,
          LitBool False <$ symbol TokFalse,
          LitInt <$> integer,
          LitChar <$> character
        ]
    )
    <?> "literal"

upperName :: Parser Text
upperName = extract p
  where
    p (TokUpperName s) = Just s
    p _ = Nothing

upper :: Parser Name
upper =
  withMaybeRange (Name <$> upperName)
    <?> "identifier that starts with an uppercase letter"

lowerName :: Parser Text
lowerName = extract p
  where
    p (TokLowerName s) = Just s
    p _ = Nothing

lower :: Parser Name
lower =
  withMaybeRange (Name <$> lowerName)
    <?> "identifier that starts with a lowercase letter"

identifier :: Parser Name
identifier =
  withMaybeRange (choice [Name <$> lowerName, Name <$> upperName]) <?> "identifier"

integer :: Parser Int
integer = extract p <?> "integer"
  where
    p (TokInt s) = Just s
    p _ = Nothing

character :: Parser Char
character = extract p <?> "character"
  where
    p (TokChar c) = Just c
    p _ = Nothing

sepBy' :: Parser (Token sep) -> Parser a -> Parser (SepBy sep a)
sepBy' delim parser = do
  x <- parser

  let f = return (Head x)
  let g = do
        sep <- delim
        xs <- sepBy' delim parser
        return $ Delim x sep xs
  try g <|> f

sepByComma :: Parser a -> Parser (SepBy "," a)
sepByComma = sepBy' tokenComma
