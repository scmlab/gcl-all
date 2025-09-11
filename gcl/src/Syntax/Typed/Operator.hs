module Syntax.Typed.Operator where

import Data.Loc
  ( Loc (..),
    (<-->),
  )
import Data.Text (Text)
import Syntax.Abstract.Operator (tBool, tFunc, tInt)
import Syntax.Abstract.Types (Lit (..), Type (..))
import Syntax.Common
import Syntax.Typed.Types
import Syntax.Typed.Util (typeOf)
import Prelude hiding (Ordering (..))

unary :: ArithOp Loc -> Type a -> Expr a -> Expr a
unary op t x = App (Op (ArithOp op) t) x (x <--> op)

arith :: ArithOp Loc -> Type a -> Expr a -> Expr a -> Expr a
arith op t x y = App (App (Op (ArithOp op) t) x (x <--> op)) y (x <--> y)

chain :: ChainOp Loc -> Type a -> Expr a -> Expr a -> Expr a -- TODO: This might be wrong. Needs further investigation.
chain op t x y = Chain (More (Pure x) (ChainOp op) t y)

neg :: Expr a -> Expr a
neg = unary (NegU ()) (tBool `tFunc` tBool)

-- Type of binary logic operators: Bool -> Bool -> Bool
-- Type of binary Int operators: Int -> Int -> Int, etc.
-- Because they are used in many occassions.

tBinLogicOp :: Type a
tBinLogicOp = tBool `tFunc` (tBool `tFunc` tBool)

tBinIntOp :: Type a
tBinIntOp = tInt `tFunc` (tInt `tFunc` tInt)

tBinIntROp :: Type a
tBinIntROp = tInt `tFunc` (tInt `tFunc` tBool)

lt, gt, gte, lte :: Expr a -> Expr a -> Expr a
lt = chain (LT ()) tBinIntROp
gt = chain (GT ()) tBinIntROp
gte = chain (GTEU ()) tBinIntROp
lte = chain (LTEU ()) tBinIntROp

conj, disj, implies :: Expr a -> Expr a -> Expr a
conj = arith (ConjU ()) tBinLogicOp
disj = arith (DisjU ()) tBinLogicOp
implies = arith (ImpliesU ()) tBinLogicOp

add :: Expr a -> Expr a -> Expr a
add = arith (Add ()) tBinIntOp

eqq :: Expr a -> Expr a -> Expr a
e0 `eqq` e1 =
  Chain (More (Pure e0) (ChainOp (EQ ())) (typeOf e0) e1)

true, false :: Expr a
true = Lit (Bol True) tBool ()
false = Lit (Bol False) tBool ()

conjunct :: [Expr a] -> Expr a
conjunct [] = true
conjunct xs = foldl1 conj xs

disjunct :: [Expr a] -> Expr a
disjunct [] = false
disjunct xs = foldl1 disj xs

predEq :: Expr a -> Expr a -> Bool
predEq = (==)

constant :: Text -> Type a -> Expr a
constant x t = Const (Name x ()) t ()

variable :: Text -> Type a -> Expr a
variable x t = Var (Name x ()) t ()

nameVar :: Name a -> Type a -> Expr a
nameVar x t = Var x t ()

number :: Int -> Expr a
number n = Lit (Num n) tInt ()

exists :: [Name a] -> Expr a -> Expr a -> Expr a
exists xs ran term =
  Quant
    (Op (ArithOp (DisjU ())) tBinLogicOp)
    xs
    ran
    term
    ()

forAll :: [Name a] -> Expr a -> Expr a -> Expr a
forAll xs ran term =
  Quant
    (Op (ArithOp (ConjU ())) tBinLogicOp)
    xs
    ran
    term
    ()

pointsTo, sConj, sImp :: Expr a -> Expr a -> Expr a
pointsTo = arith (PointsTo ()) tBinIntOp
sConj = arith (SConj ()) tBinLogicOp
sImp = arith (SImp ()) tBinLogicOp

sconjunct :: [Expr a] -> Expr a
sconjunct [] = true
sconjunct xs = foldl1 sConj xs
