module Syntax.AbstractLN.Operator where

import GCL.Range (MaybeRanged (maybeRangeOf), (<--->))
import Syntax.AbstractLN
  ( Chain (..),
    Expr (..),
    Lit (..),
    Type (..), Pattern (PattConstructor),
  )
import Syntax.Common.BuiltInOp
import Prelude hiding (Ordering (..))

-- | Constructors
unary :: Op -> Expr -> Expr
unary op x = App (Lit (Op op) Nothing) x (maybeRangeOf x)

-- arith :: ArithOp -> Expr -> Expr -> Expr
-- arith op x y = App (App (Op op) x (maybeRangeOf x <---> maybeRangeOf op)) y (maybeRangeOf x <---> maybeRangeOf y)
--
-- chain :: ChainOp -> Expr -> Expr -> Expr -- TODO: This might be wrong. Needs further investigation.
-- chain op x y = Chain (More (Pure x (maybeRangeOf x <---> maybeRangeOf op)) op y (maybeRangeOf x <---> maybeRangeOf y))
binOp :: Op -> Expr -> Expr -> Expr
binOp op e0 e1 = App (App (Lit (Op op) Nothing) e0 (maybeRangeOf e0))
                      e1 (maybeRangeOf e0 <---> maybeRangeOf e1)

lt, gt, gte, lte, eqq, conj, disj, implies, add, sub :: Expr -> Expr -> Expr
lt = binOp LT
gt = binOp GT
gte = binOp GTE
lte = binOp LTE
eqq = binOp EQ
conj = binOp Conj
disj = binOp Disj
implies = binOp Implies
add = binOp Add
sub = binOp Sub

neg :: Expr -> Expr
neg = unary Neg

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

predEq :: Expr -> Expr -> Bool
predEq = (==)

-- constant :: Text -> Expr
-- constant x = Const (Name x Nothing) Nothing
--
-- variable :: Text -> Expr
-- variable x = Var (Name x Nothing) Nothing

-- nameVar :: Name -> Expr
-- nameVar x = Var x Nothing

litOp :: Op -> Expr
litOp op = Lit (Op op) Nothing

number :: Int -> Expr
number n = Lit (Num n) Nothing

arridx :: Expr -> Expr -> Expr
arridx arr idx =
  App (App (Lit (Op ArrIdx) Nothing) arr (maybeRangeOf arr)) idx Nothing

-- exists :: [Name] -> Expr -> Expr -> Expr
-- exists xs ran term = Quant (Op (DisjU Nothing)) xs ran term Nothing
--
-- forAll :: [Name] -> Expr -> Expr -> Expr
-- forAll xs ran term = Quant (Op (ConjU Nothing)) xs ran term Nothing

pointsTo, sConj, sImp :: Expr -> Expr -> Expr
pointsTo = binOp PointsTo
sConj = binOp SConj
sImp = binOp SImp

sconjunct :: [Expr] -> Expr
sconjunct [] = true
sconjunct xs = foldl1 sConj xs


pr :: Expr -> Expr -> Expr
pr = binOp Pr

mkTuple :: [Expr] -> Expr
mkTuple [] = error "SCM: for now, tuples are non-empty"
mkTuple [x] = x
mkTuple (x:xs) = x `pr` mkTuple xs

pattPair :: Pattern -> Pattern -> Pattern
pattPair p0 p1 =
  PattConstructor (Left (Op Pr)) Nothing [p0, p1]

mkPattTuple :: [Pattern] -> Pattern
mkPattTuple [] = error "SCM: for now, tuples are non-empty"
mkPattTuple [p] = p
mkPattTuple (p:ps) = p `pattPair` mkPattTuple ps
