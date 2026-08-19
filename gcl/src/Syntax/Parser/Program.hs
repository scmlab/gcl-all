{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Program where

import qualified Data.Either as Either
import qualified Hack
import Syntax.Common (Name)
import Syntax.Concrete hiding (Op)
import Syntax.Parser.Basics
import Syntax.Parser.Definition
import Syntax.Parser.Stmt
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

--------------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------------

program :: Parser Program
program = do
  mixed <-
    sepByAlignmentOrSemi
      ( choice
          [ Left <$> declOrDefnBlock,
            Right <$> statement program
          ]
      )

  let (decls, stmts) = Either.partitionEithers mixed

  return $ Program decls stmts

declOrDefnBlock :: Parser (Hack.Choice3 Declaration DefinitionBlock Procedure)
declOrDefnBlock =
  choice
    [ Hack.A <$> declaration <?> "declaration",
      Hack.B <$> definitionBlock <?> "definition block",
      Hack.C <$> procedure <?> "procedure"
    ]

--------------------------------------------------------------------------------
-- Declaration
--------------------------------------------------------------------------------

declaration :: Parser Declaration
declaration = choice [constDecl, varDecl] <?> "declaration"

constDecl :: Parser Declaration
constDecl = ConstDecl <$> tokenConst <*> declType identifier

varDecl :: Parser Declaration
varDecl = VarDecl <$> tokenVar <*> declType identifier

-- `n : type` | `n : type { expr }`
declType :: Parser Name -> Parser DeclType
declType name = DeclType <$> declBase name <*> optional declProp

procedure :: Parser Procedure
procedure = Procedure <$> tokenProc <*> identifier <*> procParam <*> declProp <*> procBlock <*> declProp

procParam :: Parser ProcParam
procParam = ProcParam <$> tokenParenOpen <*> sepByComma procDecl <*> tokenParenClose

procDecl :: Parser ProcDecl
procDecl = choice [pVarDecl, pValueDecl] <?> "procedure declaration"

pVarDecl :: Parser ProcDecl
pVarDecl = PVarDecl <$> tokenVar <*> declBase identifier

pValueDecl :: Parser ProcDecl
pValueDecl = PValueDecl <$> tokenValue <*> declBase identifier

procBlock :: Parser ProcBlock
procBlock = ProcBlock <$> tokenBlockOpen <*> program <*> tokenBlockClose
