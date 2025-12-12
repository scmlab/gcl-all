{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GCL.Type2.MiniAst where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Loc.Range (MaybeRanged (maybeRangeOf), Range)
import Syntax.Abstract.Instances.Located ()
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (ArithOp, ChainOp, Name)

class Transform a b where
  toMiniAst :: a -> b
  fromMiniAst :: b -> a

instance Transform A.Type (Type (Maybe Range)) where
  toMiniAst (A.TBase base loc) = TBase loc base
  toMiniAst (A.TVar name loc) = TVar loc name
  toMiniAst ty = error $ show ty <> " not handled"

  fromMiniAst (TBase loc base) = A.TBase base loc
  fromMiniAst (TVar loc name) = A.TVar name loc
  fromMiniAst (TTuple _loc exprs) = A.TTuple (length exprs)
  fromMiniAst ty = error $ show ty <> " not handled"

instance Transform A.Expr (Expr (Maybe Range)) where
  toMiniAst (A.Lit lit loc) = ELit loc lit
  toMiniAst (A.Var name loc) = EVar loc name
  toMiniAst (A.Op op) = EOp (maybeRangeOf op) op
  toMiniAst (A.Chain chain) = EChain (maybeRangeOf chain) (toMiniAst chain)
  toMiniAst (A.App e1 e2 loc) = EApp loc (toMiniAst e1) (toMiniAst e2)
  toMiniAst (A.Lam name expr loc) = ELam loc name (toMiniAst expr)
  toMiniAst (A.Tuple exprs) = ETuple Nothing (NE.map toMiniAst $ NE.fromList exprs)
  toMiniAst (A.ArrIdx e1 e2 loc) = EArrIdx loc (toMiniAst e1) (toMiniAst e2)
  toMiniAst (A.ArrUpd e1 e2 e3 loc) = EArrUpd loc (toMiniAst e1) (toMiniAst e2) (toMiniAst e3)
  toMiniAst expr = error $ show expr <> " not handled"

  fromMiniAst (ELit loc lit) = A.Lit lit loc
  fromMiniAst (EVar loc name) = A.Var name loc
  fromMiniAst (EOp _loc op) = A.Op op
  fromMiniAst (EChain _loc chain) = A.Chain (fromMiniAst chain)
  fromMiniAst (EApp loc e1 e2) = A.App (fromMiniAst e1) (fromMiniAst e2) loc
  fromMiniAst (ELam loc name expr) = A.Lam name (fromMiniAst expr) loc
  fromMiniAst (ETuple _loc exprs) = A.Tuple (NE.toList $ NE.map fromMiniAst exprs)
  fromMiniAst (EArrIdx loc e1 e2) = A.ArrIdx (fromMiniAst e1) (fromMiniAst e2) loc
  fromMiniAst (EArrUpd loc e1 e2 e3) = A.ArrUpd (fromMiniAst e1) (fromMiniAst e2) (fromMiniAst e3) loc

-- fromMiniAst expr = error $ show expr <> " not handled"

instance Transform A.Chain (Chain (Maybe Range)) where
  toMiniAst (A.Pure _ _) = error "Chain should have more than 1"
  toMiniAst (A.More (A.Pure e1 _l1) op e2 l2) = Pure l2 (toMiniAst e1) op (toMiniAst e2)
  toMiniAst (A.More chain op expr loc) = More loc (toMiniAst chain) op (toMiniAst expr)

  fromMiniAst (Pure loc e1 op e2) = A.More (A.Pure e1' (maybeRangeOf e1')) op (fromMiniAst e2) loc
    where
      e1' = fromMiniAst e1
  fromMiniAst (More loc chain op expr) = A.More (fromMiniAst chain) op (fromMiniAst expr) loc

data Type a
  = TBase a A.TBase
  | TArrow a (Type a) (Type a)
  | TTuple a [Type a]
  | TVar a Name
  deriving (Show)

-- data Chain a = Chain (NonEmpty (Expr a, ChainOp)) (Expr a)
-- c :: Chain a -> b
-- c (Chain xs x)
--   | length xs == 1 = undefined
--   | otherwise = _ $ NE.head xs

-- XXX: i think this design is cursed
data Chain a
  = More a (Chain a) ChainOp (Expr a)
  | Pure a (Expr a) ChainOp (Expr a)
  deriving (Show)

data Expr a
  = ELit a A.Lit
  | EVar a Name
  | EOp a ArithOp
  | EChain a (Chain a)
  | EApp a (Expr a) (Expr a)
  | ELam a Name (Expr a)
  | ETuple a (NonEmpty (Expr a))
  | EArrIdx a (Expr a) (Expr a) -- x = arr[i]
  | EArrUpd a (Expr a) (Expr a) (Expr a) -- arr[i] = x
  deriving (Show)
