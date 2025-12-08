{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Syntax.Parser.Lexer
  ( scan,
    LexicalError,
    Tok (..),
    TokStream,
  )
where

import Data.Char
  ( isAlphaNum,
    isDigit,
    isLower,
    isSpace,
    isUpper,
  )
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Loc as Inc
import Data.Loc.Range (Pos (..), R (..), Range, fromInclusiveLoc, mkRange, unR)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Lexer.Applicative (Lexer, TokenStream (..), longest, longestShortest, runLexer, token, whitespace)
import Syntax.Parser.TokenStream (PrettyToken (..))
import Text.Regex.Applicative (Alternative (many, some, (<|>)), RE, anySym, psym, string)

--------------------------------------------------------------------------------
{-
Main components of this module:
  - 'scan': Turning the source into a token stream, according to the only 'Lexer' of this module, 'lexer'.
  - 'lexer': Defined with the set of valid tokens of gcl (the type 'Tok') and
      their recognizers (the type 'RE Char Tok', defining how tokens are recognized from an input string).

  The form of the token stream: 'TokenStream (L Tok)' were decided by 'runLexer''s return type: 'TokenStream (L tok)'.
-}
--------------------------------------------------------------------------------

-- | Tok & TokStream
data Tok
  = -- the following tokens will not be sent to the parser
    TokEOF -- might not actually be used?
  | ErrTokIndent Tok Tok Int -- Args: the token to indent to, the line number of the second Tok
  -- This token is only generated when a parser failed, and is used for error messages,
  -- it does not appear in the stream.
  | -- keywords
    TokSkip
  | TokAbort
  | TokDo
  | TokOd
  | TokIf
  | TokFi
  | TokBnd
  | TokQM -- question mark ? for digging holes
  | TokVar
  | TokCon
  | TokData
  | TokArray
  | TokOf
  | TokNew
  | TokDispose
  | TokRange
  | TokGuardBar
  | TokArrow
  | TokArrowU
  | TokCase
  | -- delimiters
    TokComma
  | TokColon
  | TokSemi
  | TokAssign
  | TokSpecOpen
  | TokSpecClose
  | TokParenOpen
  | TokParenClose
  | TokBracketOpen
  | TokBracketClose
  | TokBraceOpen
  | TokBraceClose
  | TokQuantOpen
  | TokQuantClose
  | TokQuantOpenU
  | TokQuantCloseU
  | TokBlockOpen
  | TokBlockClose
  | TokDeclOpen
  | TokDeclClose
  | -- expression
    TokUnderscore
  | -- types
    TokIntType
  | TokBoolType
  | TokCharType
  | -- operators
    TokEQ
  | TokNEQ
  | TokNEQU
  | TokGT
  | TokGTE
  | TokGTEU
  | TokLT
  | TokLTE
  | TokLTEU
  | TokEQProp
  | TokEQPropU
  | TokImpl
  | TokImplU
  | TokConj
  | TokConjU
  | TokDisj
  | TokDisjU
  | TokNeg
  | TokNegU
  | TokAdd
  | TokSub
  | TokMul
  | TokDiv
  | TokMod
  | TokExp
  | TokMax
  | TokMin
  | TokPointsTo
  | TokPointsToU
  | TokSConj
  | TokLolipop
  | TokSum
  | TokProd
  | TokForall
  | TokExist
  | TokHash
  | TokUpperName Text
  | TokLowerName Text
  | TokInt Int
  | TokChar Char
  | TokTrue
  | TokFalse
  | -- tokens for proof block {- #anchor ...} or block comment {- ... -}
    TokProof String String String -- anchor, contents, full-text containing "{-", "-}"
  deriving (Eq, Ord)

instance Show Tok where
  show tok = case tok of
    ErrTokIndent _ indT ln -> "token not indent to '" <> show indT <> "' of line " <> show ln
    TokEOF -> ""
    TokSkip -> "skip"
    TokAbort -> "abort"
    TokDo -> "do"
    TokOd -> "od"
    TokIf -> "if"
    TokFi -> "fi"
    TokBnd -> "bnd"
    TokQM -> "?"
    TokVar -> "var"
    TokCon -> "con"
    TokData -> "data"
    TokArray -> "array"
    TokOf -> "of"
    TokNew -> "new"
    TokDispose -> "dispose"
    TokRange -> ".."
    TokGuardBar -> "|"
    TokArrow -> "->"
    TokArrowU -> "→"
    TokCase -> "case"
    TokUnderscore -> "_"
    TokComma -> ","
    TokColon -> ":"
    TokSemi -> ";"
    TokAssign -> ":="
    TokSpecOpen -> "[!"
    TokSpecClose -> "!]"
    TokParenOpen -> "("
    TokParenClose -> ")"
    TokBracketOpen -> "["
    TokBracketClose -> "]"
    TokBraceOpen -> "{"
    TokBraceClose -> "}"
    TokQuantOpen -> "<|"
    TokQuantClose -> "|>"
    TokQuantOpenU -> "⟨"
    TokQuantCloseU -> "⟩"
    TokBlockOpen -> "|["
    TokBlockClose -> "]|"
    TokDeclOpen -> "{:"
    TokDeclClose -> ":}"
    TokEQ -> "="
    TokNEQ -> "/="
    TokNEQU -> "≠"
    TokGT -> ">"
    TokGTE -> ">="
    TokGTEU -> "≥"
    TokLT -> "<"
    TokLTE -> "<="
    TokLTEU -> "≤"
    TokEQProp -> "<=>"
    TokEQPropU -> "≡"
    TokImpl -> "=>"
    TokImplU -> "⇒"
    TokConj -> "&&"
    TokConjU -> "∧"
    TokDisj -> "||"
    TokDisjU -> "∨"
    TokNeg -> "~"
    TokNegU -> "¬"
    TokAdd -> "+"
    TokSub -> "-"
    TokMul -> "*"
    TokDiv -> "/"
    TokMod -> "%"
    TokExp -> "^"
    TokMax -> "↑"
    TokMin -> "↓"
    TokPointsTo -> "|->"
    TokPointsToU -> "↦"
    TokSConj -> "٭"
    TokLolipop -> "-*"
    TokSum -> "Σ"
    TokProd -> "∏"
    TokForall -> "∀"
    TokExist -> "∃"
    TokHash -> "#"
    TokUpperName s -> Text.unpack s
    TokLowerName s -> Text.unpack s
    TokInt i -> show i
    TokChar c -> "'" <> show c <> "'"
    TokTrue -> "True"
    TokFalse -> "False"
    TokIntType -> "Int"
    TokBoolType -> "Bool"
    TokCharType -> "Char"
    TokProof s _ _ -> "{- #" ++ s ++ " ...-}"

--------------------------------------------------------------------------------

-- | Regular expressions & the lexer
tokRE :: RE Char Tok
tokRE =
  TokSkip
    <$ string "skip"
      <|> TokAbort
    <$ string "abort"
      <|> TokDo
    <$ string "do"
      <|> TokOd
    <$ string "od"
      <|> TokIf
    <$ string "if"
      <|> TokFi
    <$ string "fi"
      <|> TokBnd
    <$ string "bnd"
      <|> TokQM
    <$ string "?"
      <|> TokCon
    <$ string "con"
      <|> TokVar
    <$ string "var"
      <|> TokData
    <$ string "data"
      <|> TokArray
    <$ string "array"
      <|> TokOf
    <$ string "of"
      <|> TokNew
    <$ string "new"
      <|> TokDispose
    <$ string "dispose"
      <|> TokRange
    <$ string ".."
      <|> TokGuardBar
    <$ string "|"
      <|> TokArrow
    <$ string "->"
      <|> TokArrowU
    <$ string "→"
      <|> TokCase
    <$ string "case"
      -- delimiters
      <|> TokComma
    <$ string ","
      <|> TokColon
    <$ string ":"
      <|> TokSemi
    <$ string ";"
      <|> TokAssign
    <$ string ":="
      <|> TokSpecOpen
    <$ string "[!"
      <|> TokSpecClose
    <$ string "!]"
      <|> TokParenOpen
    <$ string "("
      <|> TokParenClose
    <$ string ")"
      <|> TokBracketOpen
    <$ string "["
      <|> TokBracketClose
    <$ string "]"
      <|> TokBraceOpen
    <$ string "{"
      <|> TokBraceClose
    <$ string "}"
      <|> TokQuantOpen
    <$ string "<|"
      <|> TokQuantClose
    <$ string "|>"
      <|> TokQuantOpenU
    <$ string "⟨"
      <|> TokQuantCloseU
    <$ string "⟩"
      <|> TokBlockOpen
    <$ string "|["
      <|> TokBlockClose
    <$ string "]|"
      <|> TokDeclOpen
    <$ string "{:"
      <|> TokDeclClose
    <$ string ":}"
      -- literals
      <|> TokUnderscore
    <$ string "_"
      <|> TokEQ
    <$ string "="
      <|> TokNEQ
    <$ string "/="
      <|> TokNEQU
    <$ string "≠"
      <|> TokGT
    <$ string ">"
      <|> TokGTE
    <$ string ">="
      <|> TokGTEU
    <$ string "≥"
      <|> TokLT
    <$ string "<"
      <|> TokLTE
    <$ string "<="
      <|> TokLTEU
    <$ string "≤"
      <|> TokEQProp
    <$ string "<=>"
      <|> TokEQPropU
    <$ string "≡"
      <|> TokImpl
    <$ string "=>"
      <|> TokImplU
    <$ string "⇒"
      <|> TokConj
    <$ string "&&"
      <|> TokConjU
    <$ string "∧"
      <|> TokDisj
    <$ string "||"
      <|> TokDisjU
    <$ string "∨"
      <|> TokNeg
    <$ string "~"
      <|> TokNegU
    <$ string "¬"
      <|> TokAdd
    <$ string "+"
      <|> TokSub
    <$ string "-"
      <|> TokMul
    <$ string "*"
      <|> TokDiv
    <$ string "/"
      <|> TokMod
    <$ string "%"
      <|> TokExp
    <$ string "^"
      <|> TokMax
    <$ string "↑"
      <|> TokMin
    <$ string "↓"
      <|> TokPointsTo
    <$ string "|->"
      <|> TokPointsToU
    <$ string "↦"
      <|> TokSConj
    <$ string "٭"
      <|> TokLolipop
    <$ string "-*"
      <|> TokSum
    <$ string "Σ"
      <|> TokProd
    <$ string "∏"
      <|> TokForall
    <$ string "∀"
      <|> TokExist
    <$ string "∃"
      <|> TokTrue
    <$ string "True"
      <|> TokFalse
    <$ string "False"
      <|> TokIntType
    <$ string "Int"
      <|> TokBoolType
    <$ string "Bool"
      <|> TokCharType
    <$ string "Char"
      <|> TokUpperName
      . Text.pack
    <$> upperNameRE
      <|> TokLowerName
      . Text.pack
    <$> lowerNameRE
      <|> TokInt
    <$> intRE
      <|> TokChar
    <$> charRE
      <|> TokHash
    <$ string "#"

-- proofBlockRE :: RE Char Tok
-- proofBlockRE = TokProofBlock
--                <$> (string "{-" *> many (psym isSpace) *> proofAnchorRE)
--                <*> untilProofBlockEnd

-- commentBlockRE :: RE Char Tok
-- commentBlockRE = TokCommentBlock <$ string "{-" <* untilProofBlockEnd

-- untilProofBlockEnd :: RE Char String
-- untilProofBlockEnd = (:) <$> anySym <*> untilProofBlockEnd
--                      <|> string "-}" *> empty

-- starts with lowercase alphabets
lowerNameRE :: RE Char String
lowerNameRE = (:) <$> psym isLower <*> many (psym isNameChar)

-- starts with uppercase alphabets
upperNameRE :: RE Char String
upperNameRE = (:) <$> psym isUpper <*> many (psym isNameChar)

intRE :: RE Char Int
intRE = read <$> some (psym isDigit)

charRE :: RE Char Char
charRE = string "'" *> psym (/= '\'') <* string "'"

-- predicates
isNewline :: Char -> Bool
isNewline '\n' = True
isNewline '\r' = True
isNewline _ = False

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_' || c == '\''

proofAnchorRE :: RE Char String
proofAnchorRE =
  -- string "#" *> ((:) <$> psym isAlphaNum <*> many (psym isAlphaNum))
  string "#" *> some (psym isAlphaNum)

makeProofTok :: (String, String) -> String -> String -> Tok
makeProofTok (preSpaces, anchor) postSpaces contents =
  TokProof anchor contents ("{-" <> preSpaces <> "#" <> anchor <> postSpaces <> contents <> "-}")

lexer :: Lexer Tok
lexer =
  mconcat
    [ -- tokens to be sent to the parser
      token (longest tokRE),
      -- Handling {- #anchor ... -} and {- ... -}
      token $
        longestShortest ((,) <$> (string "{-" *> many (psym isSpace)) <*> proofAnchorRE) $
          \opening ->
            makeProofTok opening
              <$> many (psym isSpace)
              <*> (many anySym <* string "-}"),
      whitespace $ longestShortest (string "{-") $ const (many anySym <* string "-}"),
      -- meaningless tokens that are to be dumped
      whitespace (longest $ string "--" <* many (psym (not . isNewline))),
      -- single-line comment
      whitespace (longestShortest (string "{{") (const (many anySym *> string "}}"))),
      -- old-style comment block
      whitespace (longest $ psym isSpace)
    ]

--------------------------------------------------------------------------------
type LexicalError = Inc.Pos

-- | Type alias for end-exclusive token stream (after translation)
type TokStream = TokenStream (R Tok)

-- | Type alias for end-inclusive token stream (from lexer)
type TokStreamInc = TokenStream (Inc.L Tok)

scan :: FilePath -> Text -> TokStream
scan filepath =
  translateLoc . runLexer lexer filepath . Text.unpack
  where
    -- The lexer (lexer-applicative) records tokens' ranges in end-INCLUSIVE Loc (Inc.Loc),
    -- and we use translateLoc to convert it to end-EXCLUSIVE Range.
    -- For example, for the string "AB":
    --   - end-inclusive: end position is 2 (pointing to 'B')
    --   - end-exclusive: end position is 3 (pointing past 'B')
    -- Note: lexer-applicative never produces NoLoc, so fromInclusiveLoc always returns Just.
    translateLoc :: TokStreamInc -> TokStream
    translateLoc (TsToken (Inc.L loc x) rest) = TsToken (R (convertLoc loc) x) (translateLoc rest)
    translateLoc TsEof = TsEof
    translateLoc (TsError e) = TsError e

    -- Convert end-inclusive Inc.Loc to end-exclusive Range
    -- The lexer never produces NoLoc, so we use fromJust here.
    convertLoc :: Inc.Loc -> Range
    convertLoc loc = case fromInclusiveLoc loc of
      Just range -> range
      Nothing -> error "Lexer.translateLoc: unexpected NoLoc from lexer-applicative"

-- | Instances of PrettyToken
instance PrettyToken Tok where
  prettyTokens (x :| []) =
    fromMaybe ("'" <> show (unR x) <> "'") (prettyToken' (unR x))
  prettyTokens xs = "\"" <> concatMap (f . unR) (NE.toList xs) <> "\""
    where
      f tok = case prettyToken' tok of
        Nothing -> show tok
        Just pretty -> "<" <> pretty <> ">"

-- | If the given character has a pretty representation, return that,
-- otherwise 'Nothing'. This is an internal helper.
prettyToken' :: Tok -> Maybe String
prettyToken' tok = case tok of
  TokEOF -> Just "end of file"
  _ -> Nothing
