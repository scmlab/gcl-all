{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Syntax.Common.Types where

import Control.Arrow (Arrow (second))
import Data.Function (on)
import GCL.Range (Range)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | Variables and stuff
data Name = Name Text (Maybe Range)
  deriving (Show, Generic)

-- | Compare regardless of their locations
instance Eq Name where
  (==) = (==) `on` nameToText

instance Ord Name where
  compare (Name a _) (Name b _) = compare a b

nameToText :: Name -> Text
nameToText (Name x _) = x

--------------------------------------------------------------------------------
data ChainOp
  = EQProp (Maybe Range)
  | EQPropU (Maybe Range)
  | EQ (Maybe Range)
  | NEQ (Maybe Range)
  | NEQU (Maybe Range)
  | LTE (Maybe Range)
  | LTEU (Maybe Range)
  | GTE (Maybe Range)
  | GTEU (Maybe Range)
  | LT (Maybe Range)
  | GT (Maybe Range)
  deriving (Eq, Show, Generic, Ord)

data ArithOp
  = -- logic
    Implies (Maybe Range)
  | ImpliesU (Maybe Range)
  | Conj (Maybe Range)
  | ConjU (Maybe Range)
  | Disj (Maybe Range)
  | DisjU (Maybe Range)
  | Neg (Maybe Range)
  | NegU (Maybe Range)
  | -- arithmetics
    NegNum (Maybe Range)
  | Add (Maybe Range)
  | Sub (Maybe Range)
  | Mul (Maybe Range)
  | Div (Maybe Range)
  | Mod (Maybe Range)
  | Max (Maybe Range)
  | Min (Maybe Range)
  | Exp (Maybe Range)
  | Hash (Maybe Range)
  | -- pointers and sep. logic
    PointsTo (Maybe Range) -- a |-> v
  | SConj (Maybe Range)
  | SImp (Maybe Range)
  deriving (Eq, Show, Generic, Ord)

newtype TypeOp = Arrow (Maybe Range)
  deriving (Eq, Show, Generic, Ord)

-- | Operators
data Op
  = ChainOp ChainOp
  | ArithOp ArithOp
  | -- It's debatable whether we should put type operators here.
    -- This could be seen as a hack (as it is used as a workaround in order not to change too much code),
    -- However, this might also be justified in future versions of Guabao when we don't distinguish term-level and type-level operators.
    TypeOp TypeOp
  deriving (Show, Eq, Generic, Ord)

-- | The order should be same as 'opTable' defined in Syntax.Parser
-- Except NegNum (minus), which is dealt exceptionally in parser, but the objective is to make NegNum has the same precedence as below.
precedenceOrder :: [[(Op, Fixity)]]
precedenceOrder =
  [ -- application is supposed to be here
    [ (ArithOp (Hash Nothing), Prefix),
      (ArithOp (Neg Nothing), Prefix),
      (ArithOp (NegU Nothing), Prefix),
      (ArithOp (NegNum Nothing), Prefix)
    ],
    [ (ArithOp (Exp Nothing), InfixL)
    ],
    [ (ArithOp (Mul Nothing), InfixL),
      (ArithOp (Div Nothing), InfixL),
      (ArithOp (Mod Nothing), InfixL)
    ],
    [ (ArithOp (Add Nothing), InfixL),
      (ArithOp (Sub Nothing), InfixL)
    ],
    [ (ArithOp (Max Nothing), InfixL),
      (ArithOp (Min Nothing), InfixL)
    ],
    [ (ArithOp (PointsTo Nothing), Infix)
    ],
    [ (ArithOp (SConj Nothing), InfixL)
    ],
    [ (ArithOp (SImp Nothing), InfixR)
    ],
    [ (ChainOp (EQ Nothing), InfixL),
      (ChainOp (NEQ Nothing), InfixL),
      (ChainOp (NEQU Nothing), InfixL),
      (ChainOp (LTE Nothing), InfixL),
      (ChainOp (LTEU Nothing), InfixL),
      (ChainOp (GTE Nothing), InfixL),
      (ChainOp (GTEU Nothing), InfixL),
      (ChainOp (LT Nothing), InfixL),
      (ChainOp (GT Nothing), InfixL)
    ],
    [ (ArithOp (Disj Nothing), InfixL),
      (ArithOp (DisjU Nothing), InfixL),
      (ArithOp (Conj Nothing), InfixL),
      (ArithOp (ConjU Nothing), InfixL)
    ],
    [ (ArithOp (Implies Nothing), InfixR),
      (ArithOp (ImpliesU Nothing), InfixR)
    ],
    [ (ChainOp (EQProp Nothing), InfixL),
      (ChainOp (EQPropU Nothing), InfixL)
    ],
    -- Below is a type operator and is naturally very different from other operators.
    -- It is put here because we need a way to know its fixity.
    [(TypeOp (Arrow Nothing), InfixR)]
  ]

initOrderIndex :: Int
initOrderIndex = 1

classificationMap :: Map Op (Fixity, Int)
classificationMap = Map.fromList $ concat $ zipWith f [initOrderIndex ..] precedenceOrder
  where
    f :: Int -> [(Op, Fixity)] -> [(Op, (Fixity, Int))]
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

classify :: Op -> (Fixity, Int)
classify op =
  fromMaybe (error "Operator's precedenceOrder is not completely defined.") $
    Map.lookup (wipeLoc op) classificationMap

precOf :: Op -> Int
precOf = snd . classify

sameOpSym :: Op -> Op -> Bool
sameOpSym x y = wipeLoc x == wipeLoc y

wipeLoc :: Op -> Op
wipeLoc op = case op of
  ChainOp co ->
    ChainOp
      ( case co of
          EQProp _ -> EQProp Nothing
          EQPropU _ -> EQPropU Nothing
          EQ _ -> EQ Nothing
          NEQ _ -> NEQ Nothing
          NEQU _ -> NEQU Nothing
          LTE _ -> LTE Nothing
          LTEU _ -> LTEU Nothing
          GTE _ -> GTE Nothing
          GTEU _ -> GTEU Nothing
          LT _ -> LT Nothing
          GT _ -> GT Nothing
      )
  ArithOp ao ->
    ArithOp
      ( case ao of
          Implies _ -> Implies Nothing
          ImpliesU _ -> ImpliesU Nothing
          Conj _ -> Conj Nothing
          ConjU _ -> ConjU Nothing
          Disj _ -> Disj Nothing
          DisjU _ -> DisjU Nothing
          Neg _ -> Neg Nothing
          NegU _ -> NegU Nothing
          NegNum _ -> NegNum Nothing
          Add _ -> Add Nothing
          Sub _ -> Sub Nothing
          Mul _ -> Mul Nothing
          Div _ -> Div Nothing
          Mod _ -> Mod Nothing
          Max _ -> Max Nothing
          Min _ -> Min Nothing
          Exp _ -> Exp Nothing
          Hash _ -> Hash Nothing
          PointsTo _ -> PointsTo Nothing
          SConj _ -> SConj Nothing
          SImp _ -> SImp Nothing
      )
  TypeOp (Arrow _) -> TypeOp $ Arrow Nothing

-- associative operators
-- notice that =>,-> are not associative, a->(b->c) can be shown as a->b->c, but not the case of (a->b)->c
isAssocOp :: Op -> Bool
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
