module Syntax.Abstract.Operator where

import Data.Loc
  ( Loc (..),
    locOf,
    (<-->),
  )
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
unary op x = App (Op op) x (x <--> op)

arith :: ArithOp -> Expr -> Expr -> Expr
arith op x y = App (App (Op op) x (x <--> op)) y (x <--> y)

chain :: ChainOp -> Expr -> Expr -> Expr -- TODO: This might be wrong. Needs further investigation.
chain op x y = Chain (More (Pure x (x <--> op)) op y (x <--> y))

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
true = Lit (Bol True) NoLoc

false :: Expr
false = Lit (Bol False) NoLoc

conjunct :: [Expr] -> Expr
conjunct [] = true
conjunct xs = foldl1 conj xs

disjunct :: [Expr] -> Expr
disjunct [] = false
disjunct xs = foldl1 disj xs

imply :: Expr -> Expr -> Expr
imply p q = App (App ((Op . ImpliesU) Nothing) p (locOf p)) q (locOf q)

predEq :: Expr -> Expr -> Bool
predEq = (==)

constant :: Text -> Expr
constant x = Const (Name x Nothing) NoLoc

variable :: Text -> Expr
variable x = Var (Name x Nothing) NoLoc

nameVar :: Name -> Expr
nameVar x = Var x NoLoc

number :: Int -> Expr
number n = Lit (Num n) NoLoc

exists :: [Name] -> Expr -> Expr -> Expr
exists xs ran term = Quant (Op (DisjU Nothing)) xs ran term NoLoc

forAll :: [Name] -> Expr -> Expr -> Expr
forAll xs ran term = Quant (Op (ConjU Nothing)) xs ran term NoLoc

pointsTo, sConj, sImp :: Expr -> Expr -> Expr
pointsTo = (arith . PointsTo) Nothing
sConj = (arith . SConj) Nothing
sImp = (arith . SImp) Nothing

sconjunct :: [Expr] -> Expr
sconjunct [] = true
sconjunct xs = foldl1 sConj xs

-- | Frequently Used Types / Type Operators
tBool :: Type
tBool = TBase TBool NoLoc

tInt :: Type
tInt = TBase TInt NoLoc

tFunc :: Type -> Type -> Type
s `tFunc` t = TFunc s t NoLoc
