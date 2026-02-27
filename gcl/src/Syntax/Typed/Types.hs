module Syntax.Typed.Types where

import Data.Text (Text)
import GCL.Common (Index, TypeInfo)
import GCL.Range (Range)
import GCL.Type2.Common (Env)
import Syntax.Abstract.Types (Interval, Kind, Lit (..), Pattern, TBase (..), Type (..))
import Syntax.Common.Types (Name, Op, TypeOp)

data Program
  = Program
      [Definition] -- definitions (the functional language part)
      [Declaration] -- constant and variable declarations
      [Expr] -- global properties
      [Stmt] -- main program
      (Maybe Range)
  deriving (Eq, Show)

data Definition
  = TypeDefn Name [Name] [TypeDefnCtor] (Maybe Range)
  | FuncDefnSig Name KindedType (Maybe Expr) (Maybe Range)
  | FuncDefnSig' Name Type (Maybe Range)
  | FuncDefn Name Expr
  deriving (Eq, Show)

data TypeDefnCtor = TypeDefnCtor Name [Type]
  deriving (Eq, Show)

data Declaration
  = ConstDecl [Name] Type (Maybe Expr) (Maybe Range)
  | VarDecl [Name] Type (Maybe Expr) (Maybe Range)
  deriving (Eq, Show)

data Stmt
  = Skip (Maybe Range)
  | Abort (Maybe Range)
  | Assign [Name] [Expr] (Maybe Range)
  | AAssign Expr Expr Expr (Maybe Range)
  | Assert Expr (Maybe Range)
  | LoopInvariant Expr Expr (Maybe Range)
  | Do [GdCmd] (Maybe Range)
  | If [GdCmd] (Maybe Range)
  | Spec Text Range [(Index, TypeInfo)]
  | Spec' Text Range Env
  | Proof Text Text Range
  | Alloc Name [Expr] (Maybe Range) --  p := new (e1,e2,..,en)
  | HLookup Name Expr (Maybe Range) --  x := *e
  | HMutate Expr Expr (Maybe Range) --  *e1 := e2
  | Dispose Expr (Maybe Range) --  dispose e
  | Block Program (Maybe Range)
  deriving (Eq, Show)

data GdCmd = GdCmd Expr [Stmt] (Maybe Range)
  deriving (Eq, Show)

data Expr
  = Lit Lit Type (Maybe Range)
  | Var Name Type (Maybe Range)
  | Const Name Type (Maybe Range)
  | Op Op Type
  | Chain Chain
  | App Expr Expr (Maybe Range)
  | Lam Name Type Expr (Maybe Range)
  | Quant Expr [Name] Expr Expr (Maybe Range)
  | ArrIdx Expr Expr (Maybe Range)
  | ArrUpd Expr Expr Expr (Maybe Range)
  | Case Expr [CaseClause] (Maybe Range)
  | Subst Expr [(Name, Expr)]
  | EHole Text Int Type Range
  deriving (Eq, Show)

data CaseClause = CaseClause Pattern Expr
  deriving (Eq, Show)

data Chain
  = Pure Expr
  | More Chain Op Type Expr
  deriving (Eq, Show)

data KindedType
  = TBase TBase Kind (Maybe Range)
  | TArray Interval KindedType (Maybe Range)
  | TTuple Int Kind
  | TFunc KindedType KindedType (Maybe Range)
  | TOp TypeOp Kind
  | TData Name Kind (Maybe Range)
  | TApp KindedType KindedType (Maybe Range)
  | TVar Name Kind (Maybe Range)
  | TMetaVar Name Kind (Maybe Range)
  deriving (Show, Eq)
