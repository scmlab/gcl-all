module Syntax.Abstract.Operator where

import Data.Text (Text)
import qualified Hack
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
unary :: (Hack.IsRange a) => ArithOp a -> Expr a -> Expr a
unary op x = App (Op op) x (Hack.info x Hack.<--> Hack.info op)

arith :: (Hack.IsRange a) => ArithOp a -> Expr a -> Expr a -> Expr a
arith op x y = App (App (Op op) x (Hack.info x Hack.<--> Hack.info op)) y (Hack.info x Hack.<--> Hack.info y)

chain :: (Hack.IsRange a) => ChainOp a -> Expr a -> Expr a -> Expr a -- TODO: This might be wrong. Needs further investigation.
chain op x y = Chain (More (Pure x (Hack.info x Hack.<--> Hack.info op)) op y (Hack.info x Hack.<--> Hack.info y))

lt, gt, gte, lte, eqq, conj, disj, implies, add :: (Hack.IsRange a) => Expr (Maybe a) -> Expr (Maybe a) -> Expr (Maybe a)
lt = (chain . LT) Nothing
gt = (chain . GT) Nothing
gte = (chain . GTEU) Nothing
lte = (chain . LTEU) Nothing
eqq = (chain . EQ) Nothing
conj = (arith . ConjU) Nothing
disj = (arith . DisjU) Nothing
implies = (arith . ImpliesU) Nothing
add = (arith . Add) Nothing

neg :: (Hack.IsRange a) => Expr (Maybe a) -> Expr (Maybe a)
neg = (unary . NegU) Nothing

true :: Expr (Maybe a)
true = Lit (Bol True) Nothing

false :: Expr (Maybe a)
false = Lit (Bol False) Nothing

conjunct :: (Hack.IsRange a) => [Expr (Maybe a)] -> Expr (Maybe a)
conjunct [] = true
conjunct xs = foldl1 conj xs

disjunct :: (Hack.IsRange a) => [Expr (Maybe a)] -> Expr (Maybe a)
disjunct [] = false
disjunct xs = foldl1 disj xs

imply :: Expr (Maybe a) -> Expr (Maybe a) -> Expr (Maybe a)
imply p q = App (App ((Op . ImpliesU) Nothing) p (Hack.info p)) q (Hack.info q)

predEq :: (Eq a) => Expr (Maybe a) -> Expr (Maybe a) -> Bool
predEq = (==)

constant :: Text -> Expr (Maybe a)
constant x = Const (Name x Nothing) Nothing

variable :: Text -> Expr (Maybe a)
variable x = Var (Name x Nothing) Nothing

nameVar :: Name (Maybe a) -> Expr (Maybe a)
nameVar x = Var x Nothing

number :: Int -> Expr (Maybe a)
number n = Lit (Num n) Nothing

exists :: [Name (Maybe a)] -> Expr (Maybe a) -> Expr (Maybe a) -> Expr (Maybe a)
exists xs ran term = Quant (Op (DisjU Nothing)) xs ran term Nothing

forAll :: [Name (Maybe a)] -> Expr (Maybe a) -> Expr (Maybe a) -> Expr (Maybe a)
forAll xs ran term = Quant (Op (ConjU Nothing)) xs ran term Nothing

pointsTo, sConj, sImp :: (Hack.IsRange a) => Expr (Maybe a) -> Expr (Maybe a) -> Expr (Maybe a)
pointsTo = (arith . PointsTo) Nothing
sConj = (arith . SConj) Nothing
sImp = (arith . SImp) Nothing

sconjunct :: (Hack.IsRange a) => [Expr (Maybe a)] -> Expr (Maybe a)
sconjunct [] = true
sconjunct xs = foldl1 sConj xs

-- | Frequently Used Types / Type Operators
tBool :: Type (Maybe a)
tBool = TBase TBool Nothing

tInt :: Type (Maybe a)
tInt = TBase TInt Nothing

tFunc :: Type (Maybe a) -> Type (Maybe a) -> Type (Maybe a)
s `tFunc` t = TFunc s t Nothing
