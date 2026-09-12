{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Program where

import GCL.Range (R (..))
import Syntax.Common.Types (Name)
import Syntax.Concrete.Types
  ( BlockComment (..),
    CommentContent (..),
    DeclType (..),
    Declaration (..),
    DefinitionBlock,
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
  | Statement Stmt
  | BComment BlockComment

program :: Parser Program
program = do
  mixed <-
    sepByAlignmentOrSemi
      ( choice
          [ Declaration <$> declaration <?> "declaration",
            Definition <$> definitionBlock <?> "definition block",
            Statement <$> statement program,
            BComment <$> blockComment <?> "block comment"
          ]
      )

  let (defns, decls, stmts, blocks) =
        foldr
          ( \c (defns', decls', stmts', blocks') ->
              case c of
                Definition defn -> (defn : defns', decls', stmts', blocks')
                Declaration decl -> (defns', decl : decls', stmts', blocks')
                Statement stmt -> (defns', decls', stmt : stmts', blocks')
                BComment block -> (defns', decls', stmts', block : blocks')
          )
          ([], [], [], [])
          mixed

  return $ Program defns decls stmts blocks

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

blockComment :: Parser BlockComment
blockComment =
  BlockComment
    <$> tokenBlockCommentOpen
    <*> (try proofBlock <|> comment) -- TODO: how do i disable indent requirement
    <*> tokenBlockCommentClose

proofBlock :: Parser CommentContent
proofBlock = Proof <$> proof <*> tokenProofSep <*> proofText
  where
    proof = takeWhileP (Just "proof") notTokProofSep

    notTokProofSep (R _ TokProofSep) = False
    notTokProofSep _ = True

    proofText = takeWhileP (Just "proof text") notTokBlockCommentClose

comment :: Parser CommentContent
comment = Comment <$> takeWhileP (Just "comment") notTokBlockCommentClose

notTokBlockCommentClose :: R Tok -> Bool
notTokBlockCommentClose (R _ TokBlockCommentClose) = False
notTokBlockCommentClose _ = True
