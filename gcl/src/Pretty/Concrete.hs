{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty.Concrete where

import GCL.Range ()
import Data.Text (unpack)
import Pretty.Common ()
import Pretty.Util
import Pretty.Variadic
import Prettyprinter (Pretty (pretty))
import Syntax.Common
import Syntax.Concrete
import Syntax.Parser.Lexer (Tok (..))
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | SepBy
instance (PrettyWithRange (Token sep), PrettyWithRange a) => PrettyWithRange (SepBy sep a) where
  prettyWithRange (Head x) = prettyWithRange x
  prettyWithRange (Delim x sep xs) =
    prettyWithRange x <> prettyWithRange sep <> prettyWithRange xs

-- | Tokens
instance PrettyWithRange (Token "do") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokDo) l r

instance PrettyWithRange (Token "od") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokOd) l r

instance PrettyWithRange (Token "if") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokIf) l r

instance PrettyWithRange (Token "fi") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokFi) l r

instance PrettyWithRange (Token "bnd") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokBnd) l r

instance PrettyWithRange (Token "con") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokCon) l r

instance PrettyWithRange (Token "var") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokVar) l r

instance PrettyWithRange (Token "data") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokData) l r

instance PrettyWithRange (Token "array") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokArray) l r

instance PrettyWithRange (Token "new") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokNew) l r

instance PrettyWithRange (Token "dispose") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokDispose) l r

instance PrettyWithRange (Token "..") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokRange) l r

instance PrettyWithRange (Token "|") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokGuardBar) l r

instance PrettyWithRange (Token "->") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokArrow) l r

instance PrettyWithRange (Token "→") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokArrowU) l r

instance PrettyWithRange (Token "case") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokCase) l r

instance PrettyWithRange (Token "of") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokOf) l r

instance PrettyWithRange (Token "*") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokMul) l r

instance PrettyWithRange (Token "=") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokEQ) l r

instance PrettyWithRange (Token ",") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokComma) l r

instance PrettyWithRange (Token ":") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokColon) l r

instance PrettyWithRange (Token ":=") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokAssign) l r

instance PrettyWithRange (Token "[!") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokSpecOpen) l r

instance PrettyWithRange (Token "!]") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokSpecClose) l r

instance PrettyWithRange (Token "(") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokParenOpen) l r

instance PrettyWithRange (Token ")") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokParenClose) l r

instance PrettyWithRange (Token "[") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokBracketOpen) l r

instance PrettyWithRange (Token "]") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokBracketClose) l r

instance PrettyWithRange (Token "{") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokBraceOpen) l r

instance PrettyWithRange (Token "}") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokBraceClose) l r

instance PrettyWithRange (Token "<|") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokQuantOpen) l r

instance PrettyWithRange (Token "|>") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokQuantClose) l r

instance PrettyWithRange (Token "⟨") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokQuantOpenU) l r

instance PrettyWithRange (Token "⟩") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokQuantCloseU) l r

-- instance PrettyWithRange (Token "{-") where
--   prettyWithRange (Token l r) = DocWithRange (pretty $ show TokProofOpen) l r

-- instance PrettyWithRange (Token "-}") where
--   prettyWithRange (Token l r) = DocWithRange (pretty $ show TokProofClose) l r

instance PrettyWithRange (Token "{:") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokDeclOpen) l r

instance PrettyWithRange (Token ":}") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokDeclClose) l r

instance PrettyWithRange (Token "|[") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokBlockOpen) l r

instance PrettyWithRange (Token "]|") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokBlockClose) l r

instance PrettyWithRange (Token "_") where
  prettyWithRange (Token l r) = DocWithRange (pretty $ show TokUnderscore) l r

--------------------------------------------------------------------------------

-- | Program
instance Pretty Program where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange Program where
  prettyWithRange (Program decls stmts) =
    prettyWithRange decls <> prettyWithRange stmts

--------------------------------------------------------------------------------

-- | Definition
instance Pretty DefinitionBlock where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange DefinitionBlock where
  prettyWithRange (DefinitionBlock l decls r) =
    prettyWithRange l <> prettyWithRange decls <> prettyWithRange r

instance Pretty Definition where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange Definition where
  prettyWithRange (TypeDefn dat name binders eq qdcons) =
    prettyWithRange dat
      <> prettyWithRange name
      <> prettyWithRange binders
      <> prettyWithRange eq
      <> prettyWithRange qdcons
  prettyWithRange (FuncDefnSig base prop) =
    prettyWithRange base <> maybe Empty prettyWithRange prop
  prettyWithRange (FuncDefn n args e b) =
    prettyWithRange n <> prettyWithRange args <> prettyWithRange e <> prettyWithRange b

--------------------------------------------------------------------------------

-- | Declaration
instance Pretty DeclBase where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange DeclBase where
  prettyWithRange (DeclBase names colon t) =
    prettyWithRange names <> prettyWithRange colon <> prettyWithRange t

instance Pretty DeclProp where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange DeclProp where
  prettyWithRange (DeclProp l p r) =
    prettyWithRange l <> prettyWithRange p <> prettyWithRange r

instance Pretty DeclType where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange DeclType where
  prettyWithRange (DeclType decl prop) =
    prettyWithRange decl <> maybe Empty prettyWithRange prop

instance Pretty Declaration where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange Declaration where
  prettyWithRange (ConstDecl con decl) = prettyWithRange con <> prettyWithRange decl
  prettyWithRange (VarDecl v decl) = prettyWithRange v <> prettyWithRange decl

instance Pretty TypeDefnCtor where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange TypeDefnCtor where
  prettyWithRange (TypeDefnCtor n ts) = prettyWithRange n <> prettyWithRange ts

--------------------------------------------------------------------------------

-- | Literals
instance Pretty Lit where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange Lit where
  prettyWithRange (LitBool True l) = fromDoc (Just l) (pretty $ show TokTrue)
  prettyWithRange (LitBool False l) = fromDoc (Just l) (pretty $ show TokFalse)
  prettyWithRange (LitInt n l) = fromDoc (Just l) (pretty n)
  prettyWithRange (LitChar c l) = fromDoc (Just l) ("'" <> pretty [c] <> "'")

--------------------------------------------------------------------------------

-- | Stmt
instance Pretty Stmt where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange Stmt where
  prettyWithRange (Skip l) = fromDoc (Just l) (pretty $ show TokSkip)
  prettyWithRange (Abort l) = fromDoc (Just l) (pretty $ show TokAbort)
  prettyWithRange (Assign xs a es) =
    prettyWithRange xs <> prettyWithRange a <> prettyWithRange es
  prettyWithRange (AAssign x l i r a e) =
    prettyWithRange x
      <> prettyWithRange l
      <> prettyWithRange i
      <> prettyWithRange r
      <> prettyWithRange a
      <> prettyWithRange e
  prettyWithRange (Assert l p r) =
    prettyWithRange l <> prettyWithRange p <> prettyWithRange r
  prettyWithRange (LoopInvariant l p c b bnd d r) =
    prettyWithRange l
      <> prettyWithRange p
      <> prettyWithRange c
      <> prettyWithRange b
      <> prettyWithRange bnd
      <> prettyWithRange d
      <> prettyWithRange r
  prettyWithRange (Do l gdCmds r) =
    prettyWithRange l <> prettyWithRange gdCmds <> prettyWithRange r
  prettyWithRange (If l gdCmds r) =
    prettyWithRange l <> prettyWithRange gdCmds <> prettyWithRange r
  prettyWithRange (SpecQM l) = fromDoc (Just l) (pretty $ show TokQM)
  prettyWithRange (Spec l s r) =
    prettyWithRange l
      <> prettyWithRange (map (fmap show) s)
      <> prettyWithRange r
  -- where
  -- don't show Tokens like <newline> or <indent>
  -- show' Tok = case Tok of
  --   TokNewlineAndWhitespace _ -> ""
  --   TokNewlineAndWhitespaceAndBar _ -> ""
  --   TokIndent            -> ""
  --   TokDedent            -> ""
  --   TokNewline           -> ""
  --   _ -> show Tok
  prettyWithRange (Proof _ _ whole r) =
    fromDoc (Just r) (pretty whole)
  prettyWithRange (Alloc p a n l es r) =
    prettyWithRange p
      <> prettyWithRange a
      <> prettyWithRange n
      <> prettyWithRange l
      <> prettyWithRange es
      <> prettyWithRange r
  prettyWithRange (HLookup x a s e) =
    prettyWithRange x <> prettyWithRange a <> prettyWithRange s <> prettyWithRange e
  prettyWithRange (HMutate s e1 a e2) =
    prettyWithRange s <> prettyWithRange e1 <> prettyWithRange a <> prettyWithRange e2
  prettyWithRange (Dispose l e) = prettyWithRange l <> prettyWithRange e
  prettyWithRange (Block l p r) =
    prettyWithRange l <> prettyWithRange p <> prettyWithRange r

instance Pretty GdCmd where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange GdCmd where
  prettyWithRange (GdCmd guard a body) =
    prettyWithRange guard <> prettyWithRange a <> prettyWithRange body

-- instance Pretty ProofAnchor where
--   pretty = toDoc . prettyWithRange

-- instance PrettyWithRange ProofAnchor where
--   prettyWithRange (ProofAnchor hash range) =
--     fromDoc (locOf range) (pretty (show TokHash) <> pretty hash)

-- instance PrettyWithRange TextContents where
--   prettyWithRange (TextContents text range) =
--     fromDoc (locOf range) (pretty text)

--------------------------------------------------------------------------------

-- | Expr
instance Pretty Expr where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange Expr where
  prettyWithRange expr = case handleExpr expr of
    Expect _ -> mempty
    Complete s -> s

handleExpr :: Expr -> Variadic Expr (DocWithRange ann)
handleExpr (Paren l x r) =
  return $ prettyWithRange l <> prettyWithRange x <> prettyWithRange r
handleExpr (Var x) = return $ prettyWithRange x
handleExpr (Const x) = return $ prettyWithRange x
handleExpr (Lit x) = return $ prettyWithRange x
handleExpr (Op x) = handleArithOp x
handleExpr (Chain c) = handleChain c
handleExpr (Arr arr l i r) =
  return $
    prettyWithRange arr
      <> prettyWithRange l
      <> prettyWithRange i
      <> prettyWithRange r
handleExpr (App p q) = case handleExpr p of
  Expect f -> f q
  Complete s -> do
    t <- handleExpr q
    return $ s <> t
handleExpr (Quant open op xs m r n t close) =
  return $
    prettyWithRange open
      <> prettyWithRange op
      <> prettyWithRange xs
      <> prettyWithRange m
      <> prettyWithRange r
      <> prettyWithRange n
      <> prettyWithRange t
      <> prettyWithRange close
handleExpr (Case a expr b cases) =
  return $
    prettyWithRange a
      <> prettyWithRange expr
      <> prettyWithRange b
      <> prettyWithRange cases

handleArithOp :: ArithOp -> Variadic Expr (DocWithRange ann)
handleArithOp op = case classify (ArithOp op) of -- TODO: rewrite `classify` to only handle `ArithOp`s.
  (Infix, _) -> do
    p <- var
    q <- var
    return $ prettyWithRange p <> prettyWithRange op <> prettyWithRange q
  (InfixL, _) -> do
    p <- var
    q <- var
    return $ prettyWithRange p <> prettyWithRange op <> prettyWithRange q
  (InfixR, _) -> do
    p <- var
    q <- var
    return $ prettyWithRange p <> prettyWithRange op <> prettyWithRange q
  (Prefix, _) -> do
    p <- var
    return $ prettyWithRange op <> prettyWithRange p
  (Postfix, _) -> do
    p <- var
    return $ prettyWithRange p <> prettyWithRange op

handleChain :: Chain -> Variadic Expr (DocWithRange ann)
handleChain chain = case chain of
  Pure expr -> handleExpr expr
  More ch op expr -> do
    ch' <- handleChain ch
    return $ ch' <> prettyWithRange op <> prettyWithRange expr

showWithParentheses :: Expr -> String
showWithParentheses expr = case handleExpr' expr of
  Expect _ -> error "strange case in printWithParenses"
  Complete s -> s
  where
    handleExpr' :: Expr -> Variadic Expr String
    handleExpr' (Paren _ x _) = handleExpr' x
    handleExpr' (Var x) = return $ unpack $ nameToText x
    handleExpr' (Const x) = return $ unpack $ nameToText x
    handleExpr' (Lit x) = case x of
      LitInt n _ -> return $ show n
      LitBool b _ -> return $ show b
      LitChar c _ -> return $ show c
    handleExpr' (Op x) = handleArithOp' x
    handleExpr' (Chain c) = handleChain' c
    handleExpr' (Arr arr _ i _) = do
      arrs <- handleExpr' arr
      inds <- handleExpr' i
      return $ arrs <> "[" <> inds <> "]"
    handleExpr' (App p q) = case handleExpr' p of
      Expect f -> f q
      Complete s -> do
        t <- handleExpr' q
        return $ "(" <> s <> " " <> t <> ")"
    handleExpr' q@Quant {} =
      return $ show $ pretty q
    handleExpr' c@Case {} =
      return $ show $ pretty c

    handleArithOp' :: ArithOp -> Variadic Expr String
    handleArithOp' op = case classify (ArithOp op) of
      (Infix, _) -> do
        p <- var
        q <- var
        ps <- handleExpr' p
        qs <- handleExpr' q
        return $ "(" <> ps <> show (pretty op) <> qs <> ")"
      (InfixL, _) -> do
        p <- var
        q <- var
        ps <- handleExpr' p
        qs <- handleExpr' q
        return $ "(" <> ps <> show (pretty op) <> qs <> ")"
      (InfixR, _) -> do
        p <- var
        q <- var
        ps <- handleExpr' p
        qs <- handleExpr' q
        return $ "(" <> ps <> show (pretty op) <> qs <> ")"
      (Prefix, _) -> do
        p <- var
        ps <- handleExpr' p
        return $ "(" <> show (pretty op) <> ps <> ")"
      (Postfix, _) -> do
        p <- var
        ps <- handleExpr' p
        return $ "(" <> ps <> show (pretty op) <> ")"

    handleChain' :: Chain -> Variadic Expr String
    handleChain' chain = case chain of
      Pure expr' -> handleExpr' expr'
      More ch op ex -> do
        ch' <- handleChain' ch
        ex' <- handleExpr' ex
        return $ ch' <> show (pretty op) <> ex'

--------------------------------------------------------------------------------

-- | Pattern
instance PrettyWithRange CaseClause where
  prettyWithRange (CaseClause a b c) =
    prettyWithRange a <> prettyWithRange b <> prettyWithRange c

instance Pretty Pattern where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange Pattern where
  prettyWithRange patt = case patt of
    PattLit a -> prettyWithRange a
    PattParen a b c -> prettyWithRange a <> prettyWithRange b <> prettyWithRange c
    PattBinder a -> prettyWithRange a
    PattWildcard a -> prettyWithRange a
    PattConstructor a b -> prettyWithRange a <> prettyWithRange b

--------------------------------------------------------------------------------

-- | Type
instance Pretty Type where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange Type where -- TODO: Prettyprint infix type operators correctly.
  prettyWithRange (TParen l t r) =
    prettyWithRange l <> prettyWithRange t <> prettyWithRange r
  -- DocWithRange "(" l l <> prettyWithRange t <> DocWithRange ")" m m
  prettyWithRange (TBase (TInt l)) = fromDoc (Just l) (pretty ("Int" :: String))
  prettyWithRange (TBase (TBool l)) = fromDoc (Just l) (pretty ("Bool" :: String))
  prettyWithRange (TBase (TChar l)) = fromDoc (Just l) (pretty ("Char" :: String))
  prettyWithRange (TArray l a r b) =
    prettyWithRange l <> prettyWithRange a <> prettyWithRange r <> prettyWithRange b
  prettyWithRange (TOp op) = prettyWithRange op
  prettyWithRange (TData d _) = prettyWithRange d
  prettyWithRange (TApp a b) = prettyWithRange a <> prettyWithRange b
  prettyWithRange (TMetaVar i _) = prettyWithRange i

--------------------------------------------------------------------------------

-- | Endpoint & Interval
instance PrettyWithRange EndpointOpen where
  prettyWithRange (IncludingOpening l e) = prettyWithRange l <> prettyWithRange e
  prettyWithRange (ExcludingOpening l e) = prettyWithRange l <> prettyWithRange e

instance PrettyWithRange EndpointClose where
  prettyWithRange (IncludingClosing e l) = prettyWithRange e <> prettyWithRange l
  prettyWithRange (ExcludingClosing e l) = prettyWithRange e <> prettyWithRange l

instance PrettyWithRange Interval where
  prettyWithRange (Interval a b c) =
    prettyWithRange a <> prettyWithRange b <> prettyWithRange c
