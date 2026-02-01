{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Token where

import GCL.Range
import Syntax.Concrete
import Syntax.Parser.Lexer
import Syntax.Parser.Util (symbol)
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

import Syntax.Parser.Types

-- for building parsers for tokens
adapt :: Tok -> String -> Parser (Token a)
adapt t errMsg = do
  range <- symbol t <?> errMsg
  let Range l r = range
  return $ Token l r

tokenConst :: Parser (Token "con")
tokenConst = adapt TokCon "reserved word \"con\""

tokenVar :: Parser (Token "var")
tokenVar = adapt TokVar "reserved word \"var\""

tokenData :: Parser (Token "data")
tokenData = adapt TokData "reserved word \"data\""

tokenBraceOpen :: Parser (Token "{")
tokenBraceOpen = adapt TokBraceOpen "opening curly bracket"

tokenBraceClose :: Parser (Token "}")
tokenBraceClose = adapt TokBraceClose "closing curly bracket"

tokenBracketOpen :: Parser (Token "[")
tokenBracketOpen = adapt TokBracketOpen "opening square bracket"

tokenBracketClose :: Parser (Token "]")
tokenBracketClose = adapt TokBracketClose "closing square bracket"

tokenParenOpen :: Parser (Token "(")
tokenParenOpen = adapt TokParenOpen "opening parenthesis"

tokenParenClose :: Parser (Token ")")
tokenParenClose = adapt TokParenClose "closing parenthesis"

tokenQuantOpen :: Parser (Token "<|")
tokenQuantOpen = adapt TokQuantOpen "<|"

tokenQuantOpenU :: Parser (Token "⟨")
tokenQuantOpenU = adapt TokQuantOpenU "⟨"

tokenQuantClose :: Parser (Token "|>")
tokenQuantClose = adapt TokQuantClose "|>"

tokenQuantCloseU :: Parser (Token "⟩")
tokenQuantCloseU = adapt TokQuantCloseU "⟩"

tokenSpecOpen :: Parser (Token "[!")
tokenSpecOpen = adapt TokSpecOpen "[!"

tokenSpecClose :: Parser (Token "!]")
tokenSpecClose = adapt TokSpecClose "!]"

-- tokenProofOpen :: Parser (Token "{-")
-- tokenProofOpen = adapt TokProofOpen "{-"

-- tokenProofClose :: Parser (Token "-}")
-- tokenProofClose = adapt TokProofClose "-}"

tokenBlockOpen :: Parser (Token "|[")
tokenBlockOpen = adapt TokBlockOpen "|["

tokenBlockClose :: Parser (Token "]|")
tokenBlockClose = adapt TokBlockClose "]|"

tokenDeclOpen :: Parser (Token "{:")
tokenDeclOpen = adapt TokDeclOpen "{:"

tokenDeclClose :: Parser (Token ":}")
tokenDeclClose = adapt TokDeclClose ":}"

tokenColon :: Parser (Token ":")
tokenColon = adapt TokColon "colon"

tokenSemi :: Parser (Token ";")
tokenSemi = adapt TokSemi "semi"

tokenComma :: Parser (Token ",")
tokenComma = adapt TokComma "comma"

tokenRange :: Parser (Token "..")
tokenRange = adapt TokRange ".."

tokenStar :: Parser (Token "*")
tokenStar = adapt TokMul "*"

tokenArray :: Parser (Token "array")
tokenArray = adapt TokArray "reserved word \"array\""

tokenOf :: Parser (Token "of")
tokenOf = adapt TokOf "reserved word \"of\""

tokenBnd :: Parser (Token "bnd")
tokenBnd = adapt TokBnd "reserved word \"bnd\""

tokenIf :: Parser (Token "if")
tokenIf = adapt TokIf "reserved word \"if\""

tokenFi :: Parser (Token "fi")
tokenFi = adapt TokFi "reserved word \"fi\""

tokenDo :: Parser (Token "do")
tokenDo = adapt TokDo "reserved word \"do\""

tokenOd :: Parser (Token "od")
tokenOd = adapt TokOd "reserved word \"od\""

tokenCase :: Parser (Token "case")
tokenCase = adapt TokCase "reserved word \"case\""

tokenNew :: Parser (Token "new")
tokenNew = adapt TokNew "reserved word \"new\""

tokenDispose :: Parser (Token "dispose")
tokenDispose = adapt TokDispose "reserved word \"dispose\""

tokenQuestionMark :: Parser (Token "?")
tokenQuestionMark = adapt TokQM "?"

tokenAssign :: Parser (Token ":=")
tokenAssign = adapt TokAssign ":="

tokenEQ :: Parser (Token "=")
tokenEQ = adapt TokEQ "="

tokenGuardBar :: Parser (Token "|")
tokenGuardBar = adapt TokGuardBar "|"

tokenArrow :: Parser (Either (Token "->") (Token "→"))
tokenArrow =
  choice [Left <$> adapt TokArrow "->", Right <$> adapt TokArrowU "→"]

tokenUnderscore :: Parser (Token "_")
tokenUnderscore = adapt TokUnderscore "underscore \"_\""
