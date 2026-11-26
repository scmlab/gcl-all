{-# LANGUAGE DeriveGeneric #-}

module Syntax.AbstractLN.Types
  ( module Syntax.AbstractLN.Type,
    module Syntax.AbstractLN.Types,
  )
where

import Data.Text (Text)
import GCL.Range (Range)
import GHC.Generics (Generic)
import Syntax.AbstractLN.Type
import Syntax.Common (Name)
import Syntax.Common.BuiltInOp
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | Program
data Program
  = Program
  { types :: [(Name, Int)], -- user-defined types. e.g. List, Tree
    constrs :: [(Name, Type)], -- data constructors. e.g. cons :: a -> List a -> List a
    vals :: [(Name, Maybe Type, Maybe Expr)], -- functional values. e.g. f = \x -> x + 1
    consts :: [(Name, Type)], -- GCL constants. binders.
    vars :: [(Name, Type)], -- GCL variables. binders.
    props :: [Expr], -- global properties
    prog :: [Stmt], -- main program
    pragma :: [Pragma]
  }
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

-- | Expressions
data Expr
  = Lit Lit (Maybe Range)
  | Var Var (Maybe Range)
  | Chain Chain
  | App Expr Expr (Maybe Range)
  | Lam Text Expr (Maybe Range)
  | Quant Expr [Text] Expr Expr (Maybe Range)
  | Case Expr [(Pattern, Expr)] (Maybe Range)
  | EHole Hole
  deriving (Eq, Show, Generic)

data Chain = Pure Expr (Maybe Range) | More Chain Op Expr (Maybe Range)
  deriving (Eq, Show, Generic)

var :: Var -> Expr
var v = Var v Nothing

fname :: Text -> Expr
fname x = var (FName x)

bname :: Int -> Expr
bname i = var (BName i)

--------------------------------------------------------------------------------

-- | Pattern s
data Pattern
  = PattBinder Text (Maybe Range) -- binder
  | PattWildcard Range -- matches anything
  | PattConstructor (Either Lit Text) (Maybe Range) [Pattern] -- destructs a constructor
  deriving (Eq, Show, Generic)

-- | Literals
data Lit
  = Num Int
  | Bol Bool
  | Chr Char
  | Op Op
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
