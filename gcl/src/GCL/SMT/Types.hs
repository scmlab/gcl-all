{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.SMT.Types(Convert(convert), Eval(eval), SValue(..), valueAsBool, valueAsNum) where

import Data.SBV
    ( SWord8,
      SInteger,
      SBool,
      SChar,
      SymVal(literal),
      Mergeable,
      EqSymbolic ((.==)),
      Symbolic,
      sFalse )
import GHC.Generics (Generic)
import qualified Syntax.Common.Types as C
import qualified Syntax.Abstract.Types as A
import Control.Monad.Trans.State (StateT)

class Convert a b | a -> b where
  convert :: a -> b

class Eval a where
  eval :: a -> StateT [(C.Name, SValue)] Symbolic SValue

data SValue
  = SNum SInteger
  | SBool SBool
  | SChar SChar
  | SFunc (SValue -> SValue)
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

valueAsBool :: SValue -> SBool
valueAsBool (SBool b) = b
valueAsBool _ = error "Not a bool"

valueAsNum :: SValue -> SInteger
valueAsNum (SNum i) = i
valueAsNum _ = error "Not a num"
