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
  | PattBinder Text -- binder
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
