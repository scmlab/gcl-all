module Syntax.Abstract.Operator where

import Data.Loc.Range (MaybeRanged (maybeRangeOf), (<->>))
import Data.Text (Text)
import Syntax.Abstract
  ( Chain (..),
    Expr (..),
    Lit (..),
    TBase (..),
    Type (..),
  )
import Syntax.Common
import Prelude hiding (Ordering (..))

-- | Constructors
unary :: ArithOp -> Expr -> Expr
unary op x = App (Op op) x (maybeRangeOf x <->> maybeRangeOf op)

arith :: ArithOp -> Expr -> Expr -> Expr
arith op x y = App (App (Op op) x (maybeRangeOf x <->> maybeRangeOf op)) y (maybeRangeOf x <->> maybeRangeOf y)

chain :: ChainOp -> Expr -> Expr -> Expr -- TODO: This might be wrong. Needs further investigation.
chain op x y = Chain (More (Pure x (maybeRangeOf x <->> maybeRangeOf op)) op y (maybeRangeOf x <->> maybeRangeOf y))

lt, gt, gte, lte, eqq, conj, disj, implies, add :: Expr -> Expr -> Expr
lt = (chain . LT) Nothing
gt = (chain . GT) Nothing
gte = (chain . GTEU) Nothing
lte = (chain . LTEU) Nothing
eqq = (chain . EQ) Nothing
conj = (arith . ConjU) Nothing
disj = (arith . DisjU) Nothing
implies = (arith . ImpliesU) Nothing
add = (arith . Add) Nothing

neg :: Expr -> Expr
neg = (unary . NegU) Nothing

true :: Expr
true = Lit (Bol True) Nothing

false :: Expr
false = Lit (Bol False) Nothing

conjunct :: [Expr] -> Expr
conjunct [] = true
conjunct xs = foldl1 conj xs

disjunct :: [Expr] -> Expr
disjunct [] = false
disjunct xs = foldl1 disj xs

imply :: Expr -> Expr -> Expr
imply p q = App (App ((Op . ImpliesU) Nothing) p (maybeRangeOf p)) q (maybeRangeOf q)

predEq :: Expr -> Expr -> Bool
predEq = (==)

constant :: Text -> Expr
constant x = Const (Name x Nothing) Nothing

variable :: Text -> Expr
variable x = Var (Name x Nothing) Nothing

nameVar :: Name -> Expr
nameVar x = Var x Nothing

number :: Int -> Expr
number n = Lit (Num n) Nothing

exists :: [Name] -> Expr -> Expr -> Expr
exists xs ran term = Quant (Op (DisjU Nothing)) xs ran term Nothing

forAll :: [Name] -> Expr -> Expr -> Expr
forAll xs ran term = Quant (Op (ConjU Nothing)) xs ran term Nothing

pointsTo, sConj, sImp :: Expr -> Expr -> Expr
pointsTo = (arith . PointsTo) Nothing
sConj = (arith . SConj) Nothing
sImp = (arith . SImp) Nothing

sconjunct :: [Expr] -> Expr
sconjunct [] = true
sconjunct xs = foldl1 sConj xs

-- | Frequently Used Types / Type Operators
tBool :: Type
tBool = TBase TBool Nothing

tInt :: Type
tInt = TBase TInt Nothing

tFunc :: Type -> Type -> Type
s `tFunc` t = TFunc s t Nothing
