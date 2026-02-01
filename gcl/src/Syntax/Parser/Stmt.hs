{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Stmt where

import qualified Data.Text as Text
import GCL.Range
import Syntax.Concrete hiding (Op)
import Syntax.Parser.Types
import Syntax.Parser.Lexer
import Syntax.Parser.Util
import Syntax.Parser.Token
import Syntax.Parser.Basics
import Syntax.Parser.Expr
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
-- Statement
--------------------------------------------------------------------------------

statement :: Parser Program -> Parser Stmt
statement program =
  choice
    [ skip,
      proofBlock,
      abort,
      try assertion,
      loopInvariant,
      try assignment,
      try arrayAssignment,
      try alloc,
      try lookup,
      mutate,
      dispose,
      loop program,
      conditional program,
      hole,
      spec,
      programBlock program
    ]
    <?> "statement"

-- ZERO or more statements
statements ::  Parser Program -> Parser [Stmt]
statements program = sepByAlignmentOrSemi (statement program)

-- ONE or more statements
statements1 ::  Parser Program -> Parser [Stmt]
statements1 program = sepByAlignmentOrSemi1 (statement program)

skip :: Parser Stmt
skip = withRange $ Skip <$ symbol TokSkip

abort :: Parser Stmt
abort = withRange $ Abort <$ symbol TokAbort

assertion :: Parser Stmt
assertion = Assert <$> tokenBraceOpen <*> expression <*> tokenBraceClose

loopInvariant :: Parser Stmt
loopInvariant = do
  LoopInvariant
    <$> tokenBraceOpen
    <*> predicate
    <*> tokenComma
    <*> tokenBnd
    <*> tokenColon
    <*> expression
    <*> tokenBraceClose

assignment :: Parser Stmt
assignment =
  Assign <$> sepByComma lower <*> tokenAssign <*> sepByComma expression

arrayAssignment :: Parser Stmt
arrayAssignment =
  AAssign
    <$> identifier
    <*> tokenBracketOpen
    <*> expression
    <*> tokenBracketClose
    <*> tokenAssign
    <*> expression

loop :: Parser Program -> Parser Stmt
loop program = Do <$> tokenDo <* optional tokenGuardBar <*>
                      sepByGuardBar (guardedCommand program) <*> tokenOd

conditional :: Parser Program -> Parser Stmt
conditional program = If <$> tokenIf <* optional tokenGuardBar <*>
                             sepByGuardBar (guardedCommand program) <*> tokenFi

sepByGuardBar :: Parser a -> Parser (SepBy "|" a)
sepByGuardBar = sepBy' tokenGuardBar

guardedCommand :: Parser Program -> Parser GdCmd
guardedCommand program = GdCmd <$> predicate <*> tokenArrow <*>
                                   sepByAlignmentOrSemi1 (statement program)
                                   -- blockOf statement

hole :: Parser Stmt
hole = SpecQM <$> (rangeOf <$> tokenQuestionMark)

spec :: Parser Stmt
spec =
  Spec
    <$> tokenSpecOpen
    <*> takeWhileP (Just "anything other than '!]'") notTokSpecClose
    -- Although here we directly use mega's method instead of our 'symbol' and 'extract',
    -- it is enclosed by parsers built with 'symbol'.
    <*> tokenSpecClose
  where
    notTokSpecClose :: R Tok -> Bool
    notTokSpecClose (R _ TokSpecClose) = False
    notTokSpecClose _ = True

proofBlock :: Parser Stmt
proofBlock = do
  ((proofAnchor, contents, whole), r) <- getRange $ extract extractProof
  return $ Proof (Text.pack proofAnchor) (Text.pack contents) (Text.pack whole) r
  where
    extractProof (TokProof anchor contents whole) = Just (anchor, contents, whole)
    extractProof _ = Nothing

-- proofAnchors :: Parser Stmt
-- proofAnchors =
--   Proof
--     <$> tokenProofOpen
--     <*> many proofAnchor
--     <*> tokenProofClose
--  where
--   proofAnchor :: Parser ProofAnchor
--   proofAnchor = do
--     (hash, range) <- getRange $ extract extractHash
--     skipProof
--     return $ ProofAnchor hash range

--   skipProof :: Parser ()
--   skipProof = void $ takeWhileP
--     (Just "anything other than '-]' or another proof anchor")
--     notTokProofCloseOrProofAnchor

--   notTokProofCloseOrProofAnchor :: L Tok -> Bool
--   notTokProofCloseOrProofAnchor (L _ TokProofClose     ) = False
--   notTokProofCloseOrProofAnchor (L _ (TokProofAnchor _)) = False
--   notTokProofCloseOrProofAnchor _                        = True

--   extractHash (TokProofAnchor s) = Just (Text.pack s)
--   extractHash _                  = Nothing

alloc :: Parser Stmt
alloc =
  Alloc
    <$> lower
    <*> tokenAssign
    <*> tokenNew
    <*> tokenParenOpen
    <*> sepByComma expression
    <*> tokenParenClose

lookup :: Parser Stmt
lookup = HLookup <$> lower <*> tokenAssign <*> tokenStar <*> expression

mutate :: Parser Stmt
mutate = HMutate <$> tokenStar <*> expression <*> tokenAssign <*> expression

dispose :: Parser Stmt
dispose = Dispose <$> tokenDispose <*> expression

programBlock :: Parser Program -> Parser Stmt
programBlock program =
  Block
    <$> tokenBlockOpen
    <*> program
    <*> tokenBlockClose
