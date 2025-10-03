module Syntax.Abstract.Operator where

import Data.Loc.Range (Range)
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

lt, gt, gte, lte, eqq, conj, disj, implies, add :: Expr (Maybe Range) -> Expr (Maybe Range) -> Expr (Maybe Range)
lt = (chain . LT) Nothing
gt = (chain . GT) Nothing
gte = (chain . GTEU) Nothing
lte = (chain . LTEU) Nothing
eqq = (chain . EQ) Nothing
conj = (arith . ConjU) Nothing
disj = (arith . DisjU) Nothing
implies = (arith . ImpliesU) Nothing
add = (arith . Add) Nothing

neg :: Expr (Maybe Range) -> Expr (Maybe Range)
neg = (unary . NegU) Nothing

true :: Expr (Maybe Range)
true = Lit (Bol True) Nothing

false :: Expr (Maybe Range)
false = Lit (Bol False) Nothing

conjunct :: [Expr (Maybe Range)] -> Expr (Maybe Range)
conjunct [] = true
conjunct xs = foldl1 conj xs

disjunct :: [Expr (Maybe Range)] -> Expr (Maybe Range)
disjunct [] = false
disjunct xs = foldl1 disj xs

imply :: Expr (Maybe Range) -> Expr (Maybe Range) -> Expr (Maybe Range)
imply p q = App (App ((Op . ImpliesU) Nothing) p (Hack.info p)) q (Hack.info q)

predEq :: Expr (Maybe Range) -> Expr (Maybe Range) -> Bool
predEq = (==)

constant :: Text -> Expr (Maybe Range)
constant x = Const (Name x Nothing) Nothing

variable :: Text -> Expr (Maybe Range)
variable x = Var (Name x Nothing) Nothing

nameVar :: Name (Maybe Range) -> Expr (Maybe Range)
nameVar x = Var x Nothing

number :: Int -> Expr (Maybe Range)
number n = Lit (Num n) Nothing

exists :: [Name (Maybe Range)] -> Expr (Maybe Range) -> Expr (Maybe Range) -> Expr (Maybe Range)
exists xs ran term = Quant (Op (DisjU Nothing)) xs ran term Nothing

forAll :: [Name (Maybe Range)] -> Expr (Maybe Range) -> Expr (Maybe Range) -> Expr (Maybe Range)
forAll xs ran term = Quant (Op (ConjU Nothing)) xs ran term Nothing

pointsTo, sConj, sImp :: Expr (Maybe Range) -> Expr (Maybe Range) -> Expr (Maybe Range)
pointsTo = (arith . PointsTo) Nothing
sConj = (arith . SConj) Nothing
sImp = (arith . SImp) Nothing

sconjunct :: [Expr (Maybe Range)] -> Expr (Maybe Range)
sconjunct [] = true
sconjunct xs = foldl1 sConj xs

-- | Frequently Used Types / Type Operators
tBool :: Type (Maybe Range)
tBool = TBase TBool Nothing

tInt :: Type (Maybe Range)
tInt = TBase TInt Nothing

tFunc :: Type (Maybe Range) -> Type (Maybe Range) -> Type (Maybe Range)
s `tFunc` t = TFunc s t Nothing
