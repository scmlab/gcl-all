{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.SMT.Types(Convert(convert), Eval(eval), SLit(..), SValue(..)) where

import Data.SBV
    ( SWord8,
      SInteger,
      SBool,
      SChar,
      SymVal(literal),
      Mergeable,
      EqSymbolic,
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

data SLit = SLit
  {
      tag :: SWord8,
      num :: SInteger,
      bool :: SBool,
      char :: SChar
  } deriving (Generic, Mergeable, EqSymbolic)

data SValue
  = SLiteral SLit
  | SFunc (SValue -> Symbolic SValue)

instance Convert Int SLit where
  convert i = SLit 0 (literal $ toInteger i) sFalse (literal '\0')

instance Convert Bool SLit where
  convert b = SLit 1 (literal 0) (literal b) (literal '\0')

instance Convert Char SLit where
  convert c = SLit 2 (literal 0) sFalse (literal c)

instance Convert A.Lit SLit where
  convert (A.Num n) = convert n
  convert (A.Bol b) = convert b
  convert (A.Chr c) = convert c

instance Convert SBool SLit where
  convert b = SLit 1 (literal 0) b (literal '\0')

instance Convert SInteger SLit where
  convert i = SLit 0 i sFalse (literal '\0')
