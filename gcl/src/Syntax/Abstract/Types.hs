{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}

module Syntax.Abstract.Types where

import Data.List.NonEmpty (NonEmpty)
import Data.Loc.Range (Range)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
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
data Program a
  = Program
      [Definition a] -- definitions (the functional language part)
      [Declaration a] -- constant and variable declarations
      [Expr a] -- global properties
      [Stmt a] -- main program
      a
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Definition (the functional language part)
data Definition a
  = TypeDefn (Name a) [Name a] [TypeDefnCtor a] a
  | FuncDefnSig (Name a) (Type a) (Maybe (Expr a)) a
  | FuncDefn (Name a) (Expr a)
  deriving (Eq, Show)

-- constructor of type definition
data TypeDefnCtor a = TypeDefnCtor (Name a) [Type a]
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Declaration
data Declaration a
  = ConstDecl [Name a] (Type a) (Maybe (Expr a)) a
  | VarDecl [Name a] (Type a) (Maybe (Expr a)) a
  deriving (Eq, Show)

--------------------------------------------------------------------------------
data Stmt a
  = Skip a
  | Abort a
  | Assign [Name a] [Expr a] a
  | AAssign (Expr a) (Expr a) (Expr a) a
  | Assert (Expr a) a
  | LoopInvariant (Expr a) (Expr a) a
  | Do [GdCmd a] a
  | If [GdCmd a] a
  | Spec Text a
  | Proof Text Text a
  | -- pointer operations
    Alloc (Name a) [Expr a] a --  p := new (e1,e2,..,en)
  | HLookup (Name a) (Expr a) a --  x := *e
  | HMutate (Expr a) (Expr a) a --  *e1 := e2
  | Dispose (Expr a) a --  dispose e
  | Block (Program a) a
  deriving (Eq, Show)

data GdCmd a = GdCmd (Expr a) [Stmt a] a
  deriving (Eq, Show)

-- data ProofAnchor = ProofAnchor Text Range
--   deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Kinds
data Kind a
  = KStar a
  | KFunc (Kind a) (Kind a) a
  | KMetaVar (Name a)
  deriving (Show, Generic)

instance (Eq a) => Eq (Kind a) where
  KStar _ == KStar _ = True
  KFunc l1 r1 _ == KFunc l2 r2 _ = l1 == l2 && r1 == r2
  KMetaVar name1 == KMetaVar name2 = name1 == name2
  _ == _ = False

--------------------------------------------------------------------------------

-- | Endpoint
data Endpoint a = Including (Expr a) | Excluding (Expr a) deriving (Eq, Show, Generic)

-- | Interval
data Interval a = Interval (Endpoint a) (Endpoint a) a
  deriving (Eq, Show, Generic)

-- | Base Types
data TBase = TInt | TBool | TChar
  deriving (Show, Eq, Generic)

-- | Types
data Type a
  = TBase TBase a
  | TArray (Interval a) (Type a) a -- TODO: Make this a higher-kinded type.
  -- TTuple has no srcloc info because it has no conrete syntax at the moment
  | TTuple Int -- `Int` represents the arity of the tuple.
  | TFunc (Type a) (Type a) a
  | TOp (TypeOp a)
  | TData (Name a) a
  | TApp (Type a) (Type a) a
  | TVar (Name a) a
  | TMetaVar (Name a) a
  deriving (Show, Generic)

instance (Eq a) => Eq (Type a) where
  TBase base1 _ == TBase base2 _ = base1 == base2
  TArray int1 ty1 _ == TArray int2 ty2 _ = int1 == int2 && ty1 == ty2
  TTuple i1 == TTuple i2 = i1 == i2
  TOp op1 == TOp op2 = op1 == op2
  TData name1 _ == TData name2 _ = name1 == name2
  TApp left1 right1 _ == TApp left2 right2 _ = left1 == right1 && left2 == right2
  TVar name1 _ == TVar name2 _ = name1 == name2
  TMetaVar name1 _ == TMetaVar name2 _ = name1 == name2
  _ == _ = False

--------------------------------------------------------------------------------

-- | Expressions
data Expr a
  = Lit Lit a
  | Var (Name a) a
  | Const (Name a) a
  | Op (ArithOp a)
  | Chain (Chain a)
  | App (Expr a) (Expr a) a
  | Lam (Name a) (Expr a) a
  | Func (Name a) (NonEmpty (FuncClause a)) a
  | -- Tuple has no srcloc info because it has no conrete syntax at the moment
    Tuple [Expr a]
  | Quant (Expr a) [Name a] (Expr a) (Expr a) a
  | -- The innermost part of a Redex
    -- should look something like `P [ x \ a ] [ y \ b ]`
    RedexKernel
      (Name a) -- the variable to be substituted
      (Expr a) -- the expression for substituting the variable
      (Set (Name a)) -- free variables in that expression
      -- NOTE, the expression may be some definition like "P",
      --  in that case, the free variables should be that of after it's been expanded
      (NonEmpty (Mapping a))
  | -- a list of mappings of substitutions to be displayed to users (the `[ x \ a ] [ y \ b ]` part)
    -- The order is reflected.
    -- The outermost part of a Redex
    RedexShell
      Int -- for identifying Redexes in frontend-backend interactions
      (Expr a) -- should either be `RedexKernel` or `App (App (App ... RedexKernel arg0) ... argn`
  | ArrIdx (Expr a) (Expr a) a
  | ArrUpd (Expr a) (Expr a) (Expr a) a
  | Case (Expr a) [CaseClause a] a
  deriving (Eq, Show, Generic, Foldable)

data Chain a = Pure (Expr a) a | More (Chain a) (ChainOp a) (Expr a) a
  deriving (Eq, Show, Generic, Foldable)

-- QuantOp' seems not being used at current version of abstract?
type QuantOp' a = Either (ArithOp a) (Expr a)

type Mapping a = Map Text (Expr a)

--------------------------------------------------------------------------------

-- | Pattern matching

-- pattern -> expr
data CaseClause a = CaseClause (Pattern a) (Expr a)
  deriving (Eq, Show, Generic)

-- pattern0 pattern1 pattern2 ... -> expr
data FuncClause a = FuncClause [Pattern a] (Expr a)
  deriving (Eq, Show, Generic)

data Pattern a
  = PattLit Lit
  | PattBinder (Name a) -- binder
  | PattWildcard Range -- matches anything
  | PattConstructor (Name a) [Pattern a] -- destructs a constructor
  deriving (Eq, Show, Generic)

extractBinder :: Pattern a -> [Name a]
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
