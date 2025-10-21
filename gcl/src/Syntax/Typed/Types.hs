module Syntax.Typed.Types where

import Data.Loc.Range (Range)
import Data.Text (Text)
import GCL.Common (Index, TypeInfo)
import Syntax.Abstract.Types (Interval, Kind, Lit (..), Pattern, TBase (..), Type (..))
import Syntax.Common.Types (Name, Op, TypeOp)

data Program a
  = Program
      [Definition a] -- definitions (the functional language part)
      [Declaration a] -- constant and variable declarations
      [Expr a] -- global properties
      [Stmt a] -- main program
      a
  deriving (Eq, Show)

data Definition a
  = TypeDefn (Name a) [Name a] [TypeDefnCtor a] a
  | FuncDefnSig (Name a) (KindedType a) (Maybe (Expr a)) a
  | FuncDefn (Name a) (Expr a)
  deriving (Eq, Show)

data TypeDefnCtor a = TypeDefnCtor (Name a) [Type a]
  deriving (Eq, Show)

data Declaration a
  = ConstDecl [Name a] (Type a) (Maybe (Expr a)) a
  | VarDecl [Name a] (Type a) (Maybe (Expr a)) a
  deriving (Eq, Show)

data Stmt a
  = Skip a
  | Abort a
  | Assign [Name a] [Expr a] a
  | AAssign (Expr a) (Expr a) (Expr a) a
  | Assert (Expr a) a
  | LoopInvariant (Expr a) (Expr a) a
  | Do [GdCmd a] a
  | If [GdCmd a] a
  | Spec Text Range [(Index a, TypeInfo a)]
  | Proof Text Text Range
  | Alloc (Name a) [Expr a] a --  p := new (e1,e2,..,en)
  | HLookup (Name a) (Expr a) a --  x := *e
  | HMutate (Expr a) (Expr a) a --  *e1 := e2
  | Dispose (Expr a) a --  dispose e
  | Block (Program a) a
  deriving (Eq, Show)

data GdCmd a = GdCmd (Expr a) [Stmt a] a
  deriving (Eq, Show)

data Expr a
  = Lit (Lit a) (Type a) a
  | Var (Name a) (Type a) a
  | Const (Name a) (Type a) a
  | Op (Op a) (Type a)
  | Chain (Chain a)
  | App (Expr a) (Expr a) a
  | Lam (Name a) (Type a) (Expr a) a
  | Quant (Expr a) [Name a] (Expr a) (Expr a) a
  | ArrIdx (Expr a) (Expr a) a
  | ArrUpd (Expr a) (Expr a) (Expr a) a
  | Case (Expr a) [CaseClause a] a
  | Subst (Expr a) [(Name a, Expr a)]
  deriving (Eq, Show)

data CaseClause a = CaseClause (Pattern a) (Expr a)
  deriving (Eq, Show)

data Chain a
  = Pure (Expr a)
  | More (Chain a) (Op a) (Type a) (Expr a)
  deriving (Eq, Show)

data KindedType a
  = TBase TBase (Kind a) a
  | TArray (Interval a) (KindedType a) a
  | TTuple Int (Kind a)
  | TFunc (KindedType a) (KindedType a) a
  | TOp (TypeOp a) (Kind a)
  | TData (Name a) (Kind a) a
  | TApp (KindedType a) (KindedType a) a
  | TVar (Name a) (Kind a) a
  | TMetaVar (Name a) (Kind a) a
  deriving (Show, Eq)
