{-# LANGUAGE DeriveGeneric #-}

module Syntax.Abstract.Types where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GCL.Range (Range)
import GHC.Generics (Generic)
import Syntax.Common
  ( ArithOp,
    ChainOp,
    Name,
    TypeOp,
  )
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

type Const = Text

type Var = Text

type TypeVar = Text

--------------------------------------------------------------------------------

-- | Program
data Program
  = Program
      [Definition] -- definitions (the functional language part)
      [Declaration] -- constant and variable declarations
      [Expr] -- global properties
      [Stmt] -- main program
      (Maybe Range)
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Definition (the functional language part)
data Definition
  = TypeDefn Name [Name] [TypeDefnCtor] (Maybe Range)
  | FuncDefnSig Name Type (Maybe Expr) (Maybe Range)
  | FuncDefn Name Expr
  deriving (Eq, Show)

-- constructor of type definition
data TypeDefnCtor = TypeDefnCtor Name [Type]
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Declaration
data Declaration
  = ConstDecl [Name] Type (Maybe Expr) (Maybe Range)
  | VarDecl [Name] Type (Maybe Expr) (Maybe Range)
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data Stmt
  = Skip (Maybe Range)
  | Abort (Maybe Range)
  | Assign [Name] [Expr] (Maybe Range)
  | AAssign Expr Expr Expr (Maybe Range)
  | Assert Expr (Maybe Range)
  | LoopInvariant Expr Expr (Maybe Range)
  | Do [GdCmd] (Maybe Range)
  | If [GdCmd] (Maybe Range)
  | Spec Text Range
  | Proof Text Text Range
  | -- pointer operations
    Alloc Name [Expr] (Maybe Range) --  p := new (e1,e2,..,en)
  | HLookup Name Expr (Maybe Range) --  x := *e
  | HMutate Expr Expr (Maybe Range) --  *e1 := e2
  | Dispose Expr (Maybe Range) --  dispose e
  | Block Program (Maybe Range)
  deriving (Eq, Show)

data GdCmd = GdCmd Expr [Stmt] (Maybe Range)
  deriving (Eq, Show)

-- data ProofAnchor = ProofAnchor Text Range
--   deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Kinds
data Kind
  = KStar (Maybe Range)
  | KFunc Kind Kind (Maybe Range)
  | KMetaVar Name
  deriving (Show, Generic)

instance Eq Kind where
  KStar _ == KStar _ = True
  KFunc l1 r1 _ == KFunc l2 r2 _ = l1 == l2 && r1 == r2
  KMetaVar name1 == KMetaVar name2 = name1 == name2
  _ == _ = False

--------------------------------------------------------------------------------

-- | Endpoint
data Endpoint = Including Expr | Excluding Expr deriving (Eq, Show, Generic)

-- | Interval
data Interval = Interval Endpoint Endpoint (Maybe Range)
  deriving (Eq, Show, Generic)

-- | Base Types
data TBase = TInt | TBool | TChar
  deriving (Show, Eq, Generic)

-- | Types
data Type
  = TBase TBase (Maybe Range)
  | TArray Interval Type (Maybe Range) -- TODO: Make this a higher-kinded type.
  -- TTuple has no srcloc info because it has no conrete syntax at the moment
  | TTuple Int -- `Int` represents the arity of the tuple.
  | TFunc Type Type (Maybe Range)
  | TOp TypeOp
  | TData Name (Maybe Range)
  | TApp Type Type (Maybe Range)
  | TVar Name (Maybe Range)
  | TMetaVar Name (Maybe Range)
  | TType -- "*"
  deriving (Show, Generic)

-- NOTE: i don't want to deal with template haskell right now
-- but i want to catch incomplete patterns when i add new variants in
instance Eq Type where
  TBase base1 _ == TBase base2 _ = base1 == base2
  TBase {} == _ = False
  TArray int1 ty1 _ == TArray int2 ty2 _ = int1 == int2 && ty1 == ty2
  TArray {} == _ = False
  TTuple i1 == TTuple i2 = i1 == i2
  TTuple {} == _ = False
  TFunc {} == _ = False
  TOp op1 == TOp op2 = op1 == op2
  TOp {} == _ = False
  TData name1 _ == TData name2 _ = name1 == name2
  TData {} == _ = False
  TApp left1 right1 _ == TApp left2 right2 _ = left1 == right1 && left2 == right2 -- XXX: bug?
  TApp {} == _ = False
  TVar name1 _ == TVar name2 _ = name1 == name2
  TVar {} == _ = False
  TMetaVar name1 _ == TMetaVar name2 _ = name1 == name2
  TMetaVar {} == _ = False
  TType == TType = True
  TType == _ = False

--------------------------------------------------------------------------------

-- | Expressions
data Expr
  = Lit Lit (Maybe Range)
  | Var Name (Maybe Range)
  | Const Name (Maybe Range)
  | Op ArithOp
  | Chain Chain
  | App Expr Expr (Maybe Range)
  | Lam Name Expr (Maybe Range)
  | Func Name (NonEmpty FuncClause) (Maybe Range)
  | -- Tuple has no srcloc info because it has no conrete syntax at the moment
    Tuple [Expr]
  | Quant Expr [Name] Expr Expr (Maybe Range)
  | -- The innermost part of a Redex
    -- should look something like `P [ x \ a ] [ y \ b ]`
    RedexKernel
      Name -- the variable to be substituted
      Expr -- the expression for substituting the variable
      (Set Name) -- free variables in that expression
      -- NOTE, the expression may be some definition like "P",
      --  in that case, the free variables should be that of after it's been expanded
      (NonEmpty Mapping)
  | -- a list of mappings of substitutions to be displayed to users (the `[ x \ a ] [ y \ b ]` part)
    -- The order is reflected.
    -- The outermost part of a Redex
    RedexShell
      Int -- for identifying Redexes in frontend-backend interactions
      Expr -- should either be `RedexKernel` or `App (App (App ... RedexKernel arg0) ... argn`
  | ArrIdx Expr Expr (Maybe Range)
  | ArrUpd Expr Expr Expr (Maybe Range)
  | Case Expr [CaseClause] (Maybe Range)
  deriving (Eq, Show, Generic)

data Chain = Pure Expr (Maybe Range) | More Chain ChainOp Expr (Maybe Range)
  deriving (Eq, Show, Generic)

-- QuantOp' seems not being used at current version of abstract?
type QuantOp' = Either ArithOp Expr

type Mapping = Map Text Expr

--------------------------------------------------------------------------------

-- | Pattern matching

-- pattern -> expr
data CaseClause = CaseClause Pattern Expr
  deriving (Eq, Show, Generic)

-- pattern0 pattern1 pattern2 ... -> expr
data FuncClause = FuncClause [Pattern] Expr
  deriving (Eq, Show, Generic)

data Pattern
  = PattLit Lit
  | PattBinder Name -- binder
  | PattWildcard Range -- matches anything
  | PattConstructor Name [Pattern] -- destructs a constructor
  deriving (Eq, Show, Generic)

extractBinder :: Pattern -> [Name]
extractBinder (PattLit _) = []
extractBinder (PattBinder x) = [x]
extractBinder (PattWildcard _) = []
extractBinder (PattConstructor _ xs) = xs >>= extractBinder

----------------------------------------------------------------

-- | Literals
data Lit = Num Int | Bol Bool | Chr Char
  deriving (Show, Eq, Generic)

baseTypeOfLit :: Lit -> TBase
baseTypeOfLit (Num _) = TInt
baseTypeOfLit (Bol _) = TBool
baseTypeOfLit (Chr _) = TChar

----------------------------------------------------------------
