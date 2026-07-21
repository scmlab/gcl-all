{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module GCL.SMT.Types (Convert (convert), ProofBuilder (..), ExceptableSymbolic, VarMap, BuildState, SValue (..)) where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Data.Map (Map)
import Data.SBV
import Data.SBV.Dynamic (SVal, svBool, svEqual, svInteger)
import GHC.Generics (Generic)
import qualified Syntax.Abstract.Types as A
import qualified Syntax.Common.Types as C

type ExceptableSymbolic = StateT VarMap (SymbolicT (ExceptT String IO))

type VarMap = Map C.Name SValue

type BuildState = ExceptableSymbolic

class Convert a b | a -> b where
  convert :: a -> b

class ProofBuilder a where
  buildProof :: a -> BuildState SValue

data SValue
  = SVal SVal
  | SFunc (SValue -> BuildState SValue)
  deriving (Generic)

instance Convert Int SValue where
  convert = SVal . svInteger KUnbounded . toInteger

instance Convert Bool SValue where
  convert = SVal . svBool

-- FIXME(ChAoS): Somehow svChar is not visible?
instance Convert Char SValue where
  convert c = undefined

instance Convert A.Lit SValue where
  convert (A.Num n) = convert n
  convert (A.Bol b) = convert b
  convert (A.Chr c) = convert c
