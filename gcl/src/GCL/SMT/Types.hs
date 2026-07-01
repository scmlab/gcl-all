{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.SMT.Types(Convert(convert), ProofBuilder(..), ExceptableSymbolic, VarMap, BuildState, SValue(..)) where

import Data.SBV
import GHC.Generics (Generic)
import qualified Syntax.Common.Types as C
import qualified Syntax.Abstract.Types as A
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)
import Data.Map (Map)

type ExceptableSymbolic = StateT VarMap (SymbolicT (ExceptT String IO))

type VarMap = Map C.Name SValue

type BuildState = ExceptableSymbolic

class Convert a b | a -> b where
  convert :: a -> b

class ProofBuilder a where
  buildProof :: a -> BuildState SValue

data SValue
  = SNum SInteger
  | SBool SBool
  | SChar SChar
  | SFunc (SValue -> BuildState SValue)
  deriving (Generic)

instance EqSymbolic SValue where
  SNum a .== SNum b = a .== b
  SBool a .== SBool b = a .== b
  SChar a .== SChar b = a .== b
  SFunc _ .== SFunc _ = error "unable to compare functions' equality"
  _ .== _ = sFalse

instance Convert Int SValue where
  convert = SNum . literal . toInteger

instance Convert Bool SValue where
  convert = SBool . literal

instance Convert Char SValue where
  convert = SChar . literal

instance Convert A.Lit SValue where
  convert (A.Num n) = convert n
  convert (A.Bol b) = convert b
  convert (A.Chr c) = convert c

instance Convert SInteger SValue where
  convert = SNum

instance Convert SBool SValue where
  convert = SBool
