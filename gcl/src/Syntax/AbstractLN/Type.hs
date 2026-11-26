{-# LANGUAGE DeriveGeneric #-}

-- This module defines the abstract syntax for types.
-- It is separated from Syntax.AbstractLN.Types (which defines other
-- components of the untyped abstract syntax) such that the former
-- can be used independently from the latter.

-- The type inference/checking algorithm is defined in GCL.Type/Type2.

module Syntax.AbstractLN.Type where

import Data.Text (Text)
import GCL.Range (Range)
import GHC.Generics (Generic)

-- | Variables
data Var = FName Text | BName Int
  deriving (Show, Generic)

instance Eq Var where
  FName x == FName y = x == y
  BName i == BName j = i == j
  _ == _ = False

-- | Base Types
data TBuiltIn = TInt | TBool | TChar | TArray | TArrow | TPair
  deriving (Show, Eq, Generic)

-- | Types and Type Scheme
data Type
  = TLit TBuiltIn (Maybe Range)
  | TVar Var (Maybe Range)
  | TApp Type Type (Maybe Range)
  | TForall [Text] Type (Maybe Range) -- ∀α₁, ..., αₙ. t
  | TType -- "*"
  deriving (Show, Generic)

tInt, tBool, tChar :: Type
tInt = TLit TInt Nothing
tBool = TLit TBool Nothing
tChar = TLit TChar Nothing

tArrow, tPair, tArray :: Type
tArrow = TLit TArrow Nothing
tPair = TLit TPair Nothing
tArray = TLit TArray Nothing

tvar :: Var -> Type
tvar v = TVar v Nothing

tfname :: Text -> Type
tfname t = tvar (FName t)

tbname :: Int -> Type
tbname i = tvar (BName i)

arr :: Type -> Type -> Type -- was: mkArrowType
arr t1 t2 = TApp (TApp tArrow t1 Nothing) t2 Nothing

tforall :: [Text] -> Type -> Type
tforall xs t = TForall xs t Nothing

array :: Type -> Type
array t = TApp tArray t Nothing

tapp :: Type -> Type -> Type
tapp t0 t1 = TApp t0 t1 Nothing

pair :: Type -> Type -> Type
pair t1 t2 = TApp (TApp tPair t1 Nothing) t2 Nothing

tuple :: [Type] -> Type
tuple [] = error "SCM: not wanting to create a built-in unit type yet."
tuple [t] = t
tuple (t : ts) = t `pair` tuple ts

-- SCM: not sure that the use of arr makes sense here, but...
builtinTypes :: [(TBuiltIn, Type)]
builtinTypes =
  [ (TInt, TType),
    (TBool, TType),
    (TChar, TType),
    (TArrow, TType `arr` TType),
    (TPair, TType `arr` TType `arr` TType)
  ]

-- NOTE: i don't want to deal with template haskell right now
-- but i want to catch incomplete patterns when i add new variants in

instance Eq Type where
  TVar v1 _ == TVar v2 _ = v1 == v2
  TApp t1 t2 _ == TApp t3 t4 _ = t1 == t3 && t2 == t4
  TForall xs t1 _ == TForall ys t2 _ = length xs == length ys && t1 == t2
  TType == TType = True
  _ == _ = False
