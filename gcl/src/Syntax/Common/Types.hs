{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Syntax.Common.Types where

import Control.Arrow (Arrow (second))
import Data.Function (on)
import Data.Loc
import Data.Loc.Range ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | Variables and stuff
data Name = Name Text Loc
  deriving (Show, Generic)

-- | Compare regardless of their locations
instance Eq Name where
  (==) = (==) `on` nameToText

instance Ord Name where
  compare (Name a _) (Name b _) = compare a b

nameToText :: Name -> Text
nameToText (Name x _) = x

--------------------------------------------------------------------------------
data ChainOp a
  = EQProp a
  | EQPropU a
  | EQ a
  | NEQ a
  | NEQU a
  | LTE a
  | LTEU a
  | GTE a
  | GTEU a
  | LT a
  | GT a
  deriving (Eq, Show, Generic, Ord)

data ArithOp a
  = -- logic
    Implies a
  | ImpliesU a
  | Conj a
  | ConjU a
  | Disj a
  | DisjU a
  | Neg a
  | NegU a
  | -- arithmetics
    NegNum a
  | Add a
  | Sub a
  | Mul a
  | Div a
  | Mod a
  | Max a
  | Min a
  | Exp a
  | Hash a
  | -- pointers and sep. logic
    PointsTo a -- a |-> v
  | SConj a
  | SImp a
  deriving (Eq, Show, Generic, Ord)

newtype TypeOp a = Arrow a
  deriving (Eq, Show, Generic, Ord)

-- | Operators
data Op a
  = ChainOp (ChainOp a)
  | ArithOp (ArithOp a)
  | -- It's debatable whether we should put type operators here.
    -- This could be seen as a hack (as it is used as a workaround in order not to change too much code),
    -- However, this might also be justified in future versions of Guabao when we don't distinguish term-level and type-level operators.
    TypeOp (TypeOp a)
  deriving (Show, Eq, Generic, Ord)

-- | The order should be same as 'opTable' defined in Syntax.Parser
-- Except NegNum (minus), which is dealt exceptionally in parser, but the objective is to make NegNum has the same precedence as below.
precedenceOrder :: [[(Op (), Fixity)]]
precedenceOrder =
  [ -- application is supposed to be here
    [ (ArithOp (Hash ()), Prefix),
      (ArithOp (Neg ()), Prefix),
      (ArithOp (NegU ()), Prefix),
      (ArithOp (NegNum ()), Prefix)
    ],
    [ (ArithOp (Exp ()), InfixL)
    ],
    [ (ArithOp (Mul ()), InfixL),
      (ArithOp (Div ()), InfixL),
      (ArithOp (Mod ()), InfixL)
    ],
    [ (ArithOp (Add ()), InfixL),
      (ArithOp (Sub ()), InfixL)
    ],
    [ (ArithOp (Max ()), InfixL),
      (ArithOp (Min ()), InfixL)
    ],
    [ (ArithOp (PointsTo ()), Infix)
    ],
    [ (ArithOp (SConj ()), InfixL)
    ],
    [ (ArithOp (SImp ()), InfixR)
    ],
    [ (ChainOp (EQ ()), InfixL),
      (ChainOp (NEQ ()), InfixL),
      (ChainOp (NEQU ()), InfixL),
      (ChainOp (LTE ()), InfixL),
      (ChainOp (LTEU ()), InfixL),
      (ChainOp (GTE ()), InfixL),
      (ChainOp (GTEU ()), InfixL),
      (ChainOp (LT ()), InfixL),
      (ChainOp (GT ()), InfixL)
    ],
    [ (ArithOp (Disj ()), InfixL),
      (ArithOp (DisjU ()), InfixL),
      (ArithOp (Conj ()), InfixL),
      (ArithOp (ConjU ()), InfixL)
    ],
    [ (ArithOp (Implies ()), InfixR),
      (ArithOp (ImpliesU ()), InfixR)
    ],
    [ (ChainOp (EQProp ()), InfixL),
      (ChainOp (EQPropU ()), InfixL)
    ],
    -- Below is a type operator and is naturally very different from other operators.
    -- It is put here because we need a way to know its fixity.
    [(TypeOp (Arrow ()), InfixR)]
  ]

initOrderIndex :: Int
initOrderIndex = 1

classificationMap :: Map (Op ()) (Fixity, Int)
classificationMap = Map.fromList $ concat $ zipWith f [initOrderIndex ..] precedenceOrder
  where
    f :: Int -> [(Op a, Fixity)] -> [(Op a, (Fixity, Int))]
    f n = map (second (,n))

-- classifyChainOp :: ChainOp -> Fixity
-- classifyChainOp (EQ      _) = Infix 8
-- classifyChainOp (NEQ     _) = Infix 8
-- classifyChainOp (NEQU    _) = Infix 8
-- classifyChainOp (LTE     _) = Infix 8
-- classifyChainOp (LTEU    _) = Infix 8
-- classifyChainOp (GTE     _) = Infix 8
-- classifyChainOp (GTEU    _) = Infix 8
-- classifyChainOp (LT      _) = Infix 8
-- classifyChainOp (GT      _) = Infix 8
-- classifyChainOp (EQProp  _) = Infix 8
-- classifyChainOp (EQPropU _) = Infix 8

-- classifyArithOp :: ArithOp -> Fixity
-- classifyArithOp (Hash     _) = Infix (-1)

-- classifyArithOp (Neg      _) = Prefix 1
-- classifyArithOp (NegU     _) = Prefix 1
-- classifyArithOp (NegNum   _) = Prefix 1

-- classifyArithOp (Exp      _) = InfixL 2

-- classifyArithOp (Mul      _) = InfixL 2
-- classifyArithOp (Div      _) = InfixL 2
-- classifyArithOp (Mod      _) = InfixL 2

-- classifyArithOp (Add      _) = InfixL 3
-- classifyArithOp (Sub      _) = InfixL 3
-- classifyArithOp (Max      _) = InfixL 4
-- classifyArithOp (Min      _) = InfixL 4

-- classifyArithOp (PointsTo _) = Infix 5
-- classifyArithOp (SConj    _) = InfixL 6
-- classifyArithOp (SImp     _) = Infix 7

-- classifyArithOp (Disj     _) = InfixL 9
-- classifyArithOp (DisjU    _) = InfixL 9
-- classifyArithOp (Conj     _) = InfixL 9
-- classifyArithOp (ConjU    _) = InfixL 9

-- classifyArithOp (Implies  _) = InfixR 10
-- classifyArithOp (ImpliesU _) = InfixR 10

classify :: Op a -> (Fixity, Int)
classify op =
  fromMaybe (error "Operator's precedenceOrder is not completely defined.") $
    Map.lookup (wipeLoc op) classificationMap

precOf :: Op a -> Int
precOf = snd . classify

sameOpSym :: Op a -> Op a -> Bool
sameOpSym x y = wipeLoc x == wipeLoc y

wipeLoc :: Op a -> Op ()
wipeLoc op = case op of
  ChainOp co ->
    ChainOp
      ( case co of
          EQProp _ -> EQProp ()
          EQPropU _ -> EQPropU ()
          EQ _ -> EQ ()
          NEQ _ -> NEQ ()
          NEQU _ -> NEQU ()
          LTE _ -> LTE ()
          LTEU _ -> LTEU ()
          GTE _ -> GTE ()
          GTEU _ -> GTEU ()
          LT _ -> LT ()
          GT _ -> GT ()
      )
  ArithOp ao ->
    ArithOp
      ( case ao of
          Implies _ -> Implies ()
          ImpliesU _ -> ImpliesU ()
          Conj _ -> Conj ()
          ConjU _ -> ConjU ()
          Disj _ -> Disj ()
          DisjU _ -> DisjU ()
          Neg _ -> Neg ()
          NegU _ -> NegU ()
          NegNum _ -> NegNum ()
          Add _ -> Add ()
          Sub _ -> Sub ()
          Mul _ -> Mul ()
          Div _ -> Div ()
          Mod _ -> Mod ()
          Max _ -> Max ()
          Min _ -> Min ()
          Exp _ -> Exp ()
          Hash _ -> Hash ()
          PointsTo _ -> PointsTo ()
          SConj _ -> SConj ()
          SImp _ -> SImp ()
      )
  TypeOp (Arrow _) -> TypeOp $ Arrow ()

-- associative operators
-- notice that =>,-> are not associative, a->(b->c) can be shown as a->b->c, but not the case of (a->b)->c
isAssocOp :: Op a -> Bool
isAssocOp (ArithOp (Mul _)) = True
isAssocOp (ArithOp (Add _)) = True
isAssocOp (ArithOp (Conj _)) = True
isAssocOp (ArithOp (ConjU _)) = True
isAssocOp (ArithOp (Disj _)) = True
isAssocOp (ArithOp (DisjU _)) = True
isAssocOp (ArithOp (Max _)) = True
isAssocOp (ArithOp (Min _)) = True
isAssocOp (ChainOp _) = True
isAssocOp _ = False

--------------------------------------------------------------------------------

-- | Make Loc/Pos instances of FromJSON and ToJSON
-- instance ToJSON Pos where
--   toJSON (Pos filepath line column offset) = toJSON (filepath, line, column, offset)

-- instance FromJSON Pos where
--   parseJSON v = do
--     (filepath, line, column, offset) <- parseJSON v
--     return $ Pos filepath line column offset

--------------------------------------------------------------------------------

-- | Fixity & Precedence
data Fixity = Infix | InfixR | InfixL | Prefix | Postfix
  deriving (Show, Eq)

-- --------------------------------------------------------------------------------

-- -- | For annotating the usage of unicode symbols in some constructs

-- type UseUnicodeSymbol = Bool

-- useUnicodeSymbol :: UseUnicodeSymbol
-- useUnicodeSymbol = True

-- usePlainSymbol :: UseUnicodeSymbol
-- usePlainSymbol = False
