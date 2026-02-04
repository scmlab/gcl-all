{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Definition where

import Data.Maybe (maybe)
import Syntax.Common hiding (Fixity (..))
import Syntax.Concrete hiding (Op)
import Syntax.Parser.Basics
import Syntax.Parser.Expr
import Syntax.Parser.Token
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

definition :: Parser Definition
definition = choice [try funcDefnSig, typeDefn, funcDefnF]
  where
    funcDefnSig :: Parser Definition
    funcDefnSig = FuncDefnSig <$> declBase identifier <*> optional declProp

    funcDefnF :: Parser Definition
    funcDefnF = FuncDefn <$> identifier <*> many lower <*> tokenEQ <*> expression

    -- `data T a1 a2 ... = C1 ai1 ai2 .. | C2 ... | ...`
    typeDefn :: Parser Definition
    typeDefn = TypeDefn <$> tokenData <*> upper <*> many lower <*> tokenEQ <*> sepByGuardBar typeDefnCtor

    typeDefnCtor :: Parser TypeDefnCtor
    typeDefnCtor =
      TypeDefnCtor
        <$> upper
        <*> (maybe [] (reverse . unwindTApp) <$> optional type')
      where
        unwindTApp :: Type -> [Type]
        unwindTApp (TApp ts t) = t : unwindTApp ts
        unwindTApp t = [t]

    sepByGuardBar :: Parser a -> Parser (SepBy "|" a)
    sepByGuardBar = sepBy' tokenGuardBar

definitionBlock :: Parser DefinitionBlock
definitionBlock = DefinitionBlock <$> tokenDeclOpen <*> sepByAlignmentOrSemi definition <*> tokenDeclClose

-- `n : type`
declBase :: Parser Name -> Parser DeclBase
declBase name = DeclBase <$> sepByComma name <*> tokenColon <*> type'

-- `{ expr }`
declProp :: Parser DeclProp
declProp = DeclProp <$> tokenBraceOpen <*> expression <*> tokenBraceClose
