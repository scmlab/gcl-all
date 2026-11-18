{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

   --- To replace those definitions in Syntax.Common.Types
module Syntax.Common.BuiltInOp where

import Control.Arrow (Arrow (second))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------
data Op
  =
  -- orderings
    EQProp
  | EQ
  | NEQ
  | LTE
  | GTE
  | LT
  | GT
  -- logic
  | Implies
  | Conj
  | Disj
  | Neg
  -- arithmetics
  | NegNum
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Max
  | Min
  | Exp
  | Hash
  -- pair
  | Pr
  -- array operations
  | ArrUpd
  | ArrIdx
  -- pointers and sep. logic
  | PointsTo -- a |-> v
  | SConj
  | SImp
  deriving (Eq, Show, Generic, Ord)

-- | The order should be same as 'opTable' defined in Syntax.Parser
-- Except NegNum (minus), which is dealt exceptionally in parser, but the objective is to make NegNum has the same precedence as below.
precedenceOrder :: [[(Op, Fixity)]]
precedenceOrder =
  [ -- application is supposed to be here
    [ (Hash, Prefix),
      (Neg, Prefix),
      (NegNum, Prefix)
    ],
    [ (Exp, InfixL)
    ],
    [ (Mul, InfixL),
      (Div, InfixL),
      (Mod, InfixL)
    ],
    [ (Add, InfixL),
      (Sub, InfixL)
    ],
    [ (Max, InfixL),
      (Min, InfixL)
    ],
    [ (PointsTo, Infix)
    ],
    [ (SConj, InfixL)
    ],
    [ (SImp, InfixR)
    ],
    [ (EQ, InfixL),
      (NEQ, InfixL),
      (LTE, InfixL),
      (GTE, InfixL),
      (LT, InfixL),
      (GT, InfixL)
    ],
    [ (Disj, InfixL),
      (Conj, InfixL)
    ],
    [ (Implies, InfixR)
    ],
    [ (EQProp, InfixL)
    ]
  ]

initOrderIndex :: Int
initOrderIndex = 1

isChainOp :: Op -> Bool
isChainOp EQProp = True
isChainOp EQ = True
isChainOp NEQ = True
isChainOp LTE = True
isChainOp GTE = True
isChainOp LT = True
isChainOp GT = True
isChainOp _ = False

classificationMap :: Map Op (Fixity, Int)
classificationMap = Map.fromList $ concat $ zipWith f [initOrderIndex ..] precedenceOrder
  where
    f :: Int -> [(Op, Fixity)] -> [(Op, (Fixity, Int))]
    f n = map (second (,n))

classify :: Op -> (Fixity, Int)
classify op =
  fromMaybe (error "Operator's precedenceOrder is not completely defined.") $
    Map.lookup op classificationMap

precOf :: Op -> Int
precOf = snd . classify

-- associative operators
-- notice that =>,-> are not associative, a->(b->c) can be shown as a->b->c, but not the case of (a->b)->c
isAssocOp :: Op -> Bool
isAssocOp op | isChainOp op = True
isAssocOp Mul = True
isAssocOp Add = True
isAssocOp Conj = True
isAssocOp Disj = True
isAssocOp Max = True
isAssocOp Min = True
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
