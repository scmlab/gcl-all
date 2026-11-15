{-# LANGUAGE DeriveGeneric #-}

module Syntax.AbstractLN.Types where

import Data.Text (Text)
import GCL.Range (Range)
import GHC.Generics (Generic)
import Syntax.Common
  ( ArithOp,
    ChainOp,
    Name,
    TypeOp (..),
  )
import Prelude hiding (Ordering (..))
import Documentation.SBV.Examples.ProofTools.BMC (S(x))


-- shall we use the type Syntax.Common.Name?

data Var = FName Text (Maybe Range) | BName Int (Maybe Range)
   deriving (Show, Generic)

instance Eq Var where
  FName x _ == FName y _ = x == y
  BName i _ == BName j _ = i == j
  _ == _ = False

--------------------------------------------------------------------------------

-- | Program
data Program
  = Program {
      consts :: [(Name, Type)],  -- GCL constants
      vars :: [(Name, Type)],    -- GCL variables
      props :: [Expr],           -- global properties
      prog :: [Stmt],            -- main program
      pragma :: [Pragma]
    }
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
  | Assign [(Either Var Hole, Expr)] (Maybe Range)
  | AAssign Expr Expr Expr (Maybe Range)
  | Assert Expr (Maybe Range)
  | LoopInvariant Expr Expr (Maybe Range)
  | Do [GdCmd] (Maybe Range)
  | If [GdCmd] (Maybe Range)
  | Spec Text Range
  | -- pointer operations
    Alloc Var [Expr] (Maybe Range) --  p := new (e1,e2,..,en)
  | HLookup Var Expr (Maybe Range) --  x := *e
  | HMutate Expr Expr (Maybe Range) --  *e1 := e2
  | Dispose Expr (Maybe Range) --  dispose e
--  | Block Program (Maybe Range)
  deriving (Eq, Show)

data GdCmd = GdCmd Expr [Stmt] (Maybe Range)
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Endpoint
data Endpoint = Including Expr | Excluding Expr deriving (Eq, Show, Generic)

-- | Interval
data Interval = Interval Endpoint Endpoint (Maybe Range)
  deriving (Eq, Show, Generic)

-- | Base Types
data TBase = TInt | TBool | TChar
  deriving (Show, Eq, Generic)

-- | Types and Type Scheme
data Type
  = TBase TBase (Maybe Range)
  | TArray Interval Type (Maybe Range) -- TODO: Make this a higher-kinded type.
  -- TTuple has no srcloc info because it has no conrete syntax at the moment
  | TTuple [Type] -- a list of types, for internal use
  | TOp TypeOp
  | TData Name (Maybe Range)
  | TApp Type Type (Maybe Range)
  | TVar Name (Maybe Range)
  | TMetaVar Name (Maybe Range)
  | TType -- "*"
  deriving (Show, Generic)

mkArrowType :: Type -> Type -> Type
mkArrowType argument result =
  TApp
    (TApp (TOp (Arrow Nothing)) argument Nothing)
    result
    Nothing

data Scheme
  = Forall [Name] Type -- ∀α₁, ..., αₙ. t
  deriving (Eq, Show)

-- NOTE: i don't want to deal with template haskell right now
-- but i want to catch incomplete patterns when i add new variants in
instance Eq Type where
  TBase base1 _ == TBase base2 _ = base1 == base2
  TBase {} == _ = False
  TArray int1 ty1 _ == TArray int2 ty2 _ = int1 == int2 && ty1 == ty2
  TArray {} == _ = False
  TTuple i1 == TTuple i2 = i1 == i2
  TTuple {} == _ = False
  TOp op1 == TOp op2 = eqTypeOp op1 op2
  TOp {} == _ = False
  TData name1 _ == TData name2 _ = name1 == name2
  TData {} == _ = False
  TApp left1 right1 _ == TApp left2 right2 _ = left1 == left2 && right1 == right2
  TApp {} == _ = False
  TVar name1 _ == TVar name2 _ = name1 == name2
  TVar {} == _ = False
  TMetaVar name1 _ == TMetaVar name2 _ = name1 == name2
  TMetaVar {} == _ = False
  TType == TType = True
  TType == _ = False

-- Keep this comparison exhaustive rather than adding a wildcard fallback.
-- With incomplete-pattern warnings enabled, adding a new 'TypeOp' constructor
-- then forces its equality semantics to be considered instead of silently
-- treating two values of the new constructor as unequal.
eqTypeOp :: TypeOp -> TypeOp -> Bool
eqTypeOp (Arrow _) (Arrow _) = True

--------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-- | Expressions
data Expr
  = Lit Lit (Maybe Range)
  | Var Var
  | Const Var
  | Op ArithOp
  | Chain Chain
  | App Expr Expr (Maybe Range)
  | Lam Text Expr (Maybe Range)
  | -- Tuple has no srcloc info because it has no conrete syntax at the moment
    Tuple [Expr] --- for internal use
  | OutT Int Expr --- OutT i (Tuple [x1,x2..]) = xi. For internal use.
  | Quant Expr [Text] Expr Expr (Maybe Range)
  | ArrIdx Expr Expr (Maybe Range)
  | ArrUpd Expr Expr Expr (Maybe Range)
  | Case Expr [CaseClause] (Maybe Range)
  | EHole Hole
  deriving (Eq, Show, Generic)

data Chain = Pure Expr (Maybe Range) | More Chain ChainOp Expr (Maybe Range)
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

-- | Pattern matching

-- pattern -> expr
data CaseClause = CaseClause Pattern Expr
  deriving (Eq, Show, Generic)

-- pattern0 pattern1 pattern2 ... -> expr
-- data FuncClause = FuncClause [Pattern] Expr
--   deriving (Eq, Show, Generic)

data Pattern
  = PattLit Lit
  | PattBinder Name -- binder
  | PattWildcard Range -- matches anything
  | PattTuple [Pattern]
  | PattConstructor Name [Pattern] -- destructs a constructor
  deriving (Eq, Show, Generic)

-- | Literals
data Lit = Num Int | Bol Bool | Chr Char
  deriving (Show, Eq, Generic)

----------------------------------------------------------------

-- | Hole
data Hole = Hole Text Int Range
  deriving (Eq, Show, Generic)

data Pragma
  = Proof Text Text
  deriving (Eq, Show)

----------------------------------------------------------------
-- | Classes

class LocallyNameless sntx where
   close :: [(Text, Int)] -> sntx -> sntx
   open :: [(Int, Text)] -> sntx -> sntx
