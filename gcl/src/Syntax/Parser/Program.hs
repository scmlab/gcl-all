{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Program where

import GCL.Range (R (..))
import Syntax.Common.Types (Name)
import Syntax.Concrete.Types
  ( DeclType (..),
    Declaration (..),
    DefinitionBlock,
    Pragma (..),
    PragmaContent (..),
    ProcBlock (..),
    ProcDecl (..),
    ProcParam (..),
    Procedure (..),
    Program (..),
    Stmt,
  )
import Syntax.Parser.Basics
import Syntax.Parser.Definition
import Syntax.Parser.Lexer (Tok (..))
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

data Construct
  = Definition DefinitionBlock
  | Declaration Declaration
  | Procedure' Procedure
  | Statement Stmt
  | Pragma' Pragma

program :: Parser Program
program = do
  mixed <-
    sepByAlignmentOrSemi
      ( choice
          [ Declaration <$> declaration <?> "declaration",
            Definition <$> definitionBlock <?> "definition block",
            Procedure' <$> procedure <?> "procedure block",
            Statement <$> statement program,
            Pragma' <$> pragma <?> "pragma block"
          ]
      )

  let (defns, decls, procs, stmts, blocks) =
        foldr
          ( \c (defns', decls', procs', stmts', prags') ->
              case c of
                Definition defn -> (defn : defns', decls', procs', stmts', prags')
                Declaration decl -> (defns', decl : decls', procs', stmts', prags')
                Procedure' proc -> (defns', decls', proc : procs', stmts', prags')
                Statement stmt -> (defns', decls', procs', stmt : stmts', prags')
                Pragma' prag -> (defns', decls', procs', stmts', prag : prags')
          )
          ([], [], [], [], [])
          mixed

  return $ Program defns decls procs stmts blocks

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

pragma :: Parser Pragma
pragma =
  Pragma
    <$> tokenPragmaOpen
    <*> try proofBlock
    <*> tokenPragmaClose

proofBlock :: Parser PragmaContent
proofBlock = Proof <$> proof <*> tokenProofSep <*> proofText
  where
    proof = takeWhileP (Just "proof") notTokProofSep

    notTokProofSep (R _ TokProofSep) = False
    notTokProofSep _ = True

    proofText = takeWhileP (Just "proof text") notTokPragmaClose

notTokPragmaClose :: R Tok -> Bool
notTokPragmaClose (R _ TokPragmaClose) = False
notTokPragmaClose _ = True
