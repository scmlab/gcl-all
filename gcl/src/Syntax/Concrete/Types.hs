{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Syntax.Concrete.Types where

import Data.Loc (L, Loc (Loc), Located (locOf), Pos)
import Data.Loc.Range
import Data.Text (Text)
import GHC.Base (Symbol)
import GHC.Generics (Generic)
import Syntax.Common (ArithOp, ChainOp, Name, TypeOp)
import Syntax.Parser.Lexer (Tok)
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | A Token with start & ending Pos
data Token (a :: Symbol) = Token Pos Pos
  deriving (Eq, Show)

instance Located (Token a) where
  locOf (Token l r) = Loc l r

instance Ranged (Token a) where
  rangeOf (Token l r) = Range l r

instance Ranged (Either (Token a) (Token b)) where
  rangeOf (Left x) = rangeOf x
  rangeOf (Right x) = rangeOf x

-- unicode token wraper
type TokQuantStarts = Either (Token "<|") (Token "⟨")

type TokQuantEnds = Either (Token "|>") (Token "⟩")

type TokArrows = Either (Token "->") (Token "→")

--------------------------------------------------------------------------------

-- | A non-empty list of stuff seperated by some delimeter
data SepBy (sep :: Symbol) a = Head a | Delim a (Token sep) (SepBy sep a)
  deriving (Eq, Show, Functor, Foldable)

--------------------------------------------------------------------------------

-- | Program
data Program a
  = Program
      [Either (Declaration a) (DefinitionBlock a)] -- constant and variable declarations
      [Stmt a] -- main program
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Definitions
data DefinitionBlock a = DefinitionBlock (Token "{:") [Definition a] (Token ":}") deriving (Eq, Show)

data Definition a
  = -- data T a1 a2 ... = K1 v1 v2 ... | K2 u1 u2 ...
    TypeDefn (Token "data") (Name a) [Name a] (Token "=") (SepBy "|" (TypeDefnCtor a))
  | -- f : A -> B { Prop }
    FuncDefnSig (DeclBase a) (Maybe (DeclProp a))
  | -- f a = a
    FuncDefn (Name a) [Name a] (Token "=") (Expr a)
  deriving (Eq, Show)

data TypeDefnCtor a = TypeDefnCtor (Name a) [Type a] deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Declaration
data Declaration a
  = ConstDecl (Token "con") (DeclType a)
  | VarDecl (Token "var") (DeclType a)
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- Low level Declaration wrapper, and synonym types
data DeclBase a = DeclBase (SepBy "," (Name a)) (Token ":") (Type a) deriving (Eq, Show)

data DeclProp a = DeclProp (Token "{") (Expr a) (Token "}") deriving (Eq, Show)

data DeclType a = DeclType (DeclBase a) (Maybe (DeclProp a)) deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Statements
data Stmt a
  = Skip Range
  | Abort Range
  | Assign (SepBy "," (Name a)) (Token ":=") (SepBy "," (Expr a))
  | AAssign (Name a) (Token "[") (Expr a) (Token "]") (Token ":=") (Expr a)
  | Assert (Token "{") (Expr a) (Token "}")
  | LoopInvariant (Token "{") (Expr a) (Token ",") (Token "bnd") (Token ":") (Expr a) (Token "}")
  | Do (Token "do") (SepBy "|" (GdCmd a)) (Token "od")
  | If (Token "if") (SepBy "|" (GdCmd a)) (Token "fi")
  | SpecQM Range -- ? to be rewritten as [!!]
  | Spec (Token "[!") [L Tok] (Token "!]")
  | Proof Text Text Text Range -- anchor, the content of the block, the whole proof block (for pretty's reconstruction)
  | Alloc (Name a) (Token ":=") (Token "new") (Token "(") (SepBy "," (Expr a)) (Token ")")
  | HLookup (Name a) (Token ":=") (Token "*") (Expr a)
  | HMutate (Token "*") (Expr a) (Token ":=") (Expr a)
  | Dispose (Token "dispose") (Expr a)
  | Block (Token "|[") (Program a) (Token "]|")
  deriving (Eq, Show)

data GdCmd a = GdCmd (Expr a) TokArrows [Stmt a] deriving (Eq, Show)

-- data ProofAnchor = ProofAnchor Text Range deriving (Eq, Show)
-- data TextContents = TextContents Text Range deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Endpoint
data EndpointOpen a
  = IncludingOpening (Token "[") (Expr a)
  | ExcludingOpening (Token "(") (Expr a)
  deriving (Eq, Show)

data EndpointClose a
  = IncludingClosing (Expr a) (Token "]")
  | ExcludingClosing (Expr a) (Token ")")
  deriving (Eq, Show)

-- | Interval
data Interval a = Interval (EndpointOpen a) (Token "..") (EndpointClose a) deriving (Eq, Show)

-- | Base Type
data TBase
  = TInt Range
  | TBool Range
  | TChar Range
  deriving (Eq, Show)

-- | Type
data Type a
  = TParen (Token "(") (Type a) (Token ")")
  | TBase TBase
  | TArray (Token "array") (Interval a) (Token "of") (Type a)
  | TOp (TypeOp Loc)
  | TData (Name a) Range
  | TApp (Type a) (Type a)
  | TMetaVar (Name a) Range
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Expressions
data Expr a
  = Paren (Token "(") (Expr a) (Token ")")
  | Lit Lit
  | Var (Name a)
  | Const (Name a)
  | Op (ArithOp Loc)
  | Chain (Chain a)
  | Arr (Expr a) (Token "[") (Expr a) (Token "]")
  | App (Expr a) (Expr a)
  | Quant
      TokQuantStarts
      QuantOp'
      [Name a]
      (Token ":")
      (Expr a)
      (Token ":")
      (Expr a)
      TokQuantEnds
  | -- case expr of { ctor1 -> expr | ctor2 binder1 binder2 -> expr }
    Case (Token "case") (Expr a) (Token "of") [CaseClause a]
  deriving (Eq, Show, Generic)

data Chain a = Pure (Expr a) | More (Chain a) (ChainOp Loc) (Expr a)
  deriving (Eq, Show, Generic)

type QuantOp' = Either (ArithOp Loc) (Name Loc)

--------------------------------------------------------------------------------

-- | Pattern matching

-- ctor1 binder1 binder2 ... -> expr
data CaseClause a = CaseClause (Pattern a) TokArrows (Expr a)
  deriving (Eq, Show, Generic)

-- NOTE: current not in use
data Pattern a
  = PattLit Lit
  | PattParen (Token "(") (Pattern a) (Token ")") -- pattern wrapped inside a pair of parenthesis
  | PattBinder (Name a) -- binder
  | PattWildcard (Token "_") -- matches anything
  | PattConstructor (Name a) [Pattern a] -- destructs a constructor
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Literals (Integer / Boolean / Character)
data Lit = LitInt Int Range | LitBool Bool Range | LitChar Char Range
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
