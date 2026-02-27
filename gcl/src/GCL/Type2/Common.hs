module GCL.Type2.Common where

import Data.Map (Map)
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name)

type TyVar = Name

type TmVar = Name

data Scheme
  = Forall [TyVar] A.Type -- ∀α₁, ..., αₙ. t
  deriving (Eq, Show)

type Env = Map TmVar Scheme
