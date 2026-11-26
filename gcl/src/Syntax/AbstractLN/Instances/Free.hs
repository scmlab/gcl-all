module Syntax.AbstractLN.Instances.Free where

import Data.Set (Set)
import qualified Data.Set as Set
import GCL.Common (Free (..))
import Syntax.AbstractLN.Types

instance Free Expr where
  freeVarsT (Lit _ _) = mempty
  freeVarsT (Var v _) = freeVarsT v
  freeVarsT (Chain chain) = freeVarsT chain
  freeVarsT (App e1 e2 _) = freeVarsT e1 <> freeVarsT e2
  freeVarsT (Lam _ e _) = freeVarsT e
  freeVarsT (Quant op _ range term _) =
    freeVarsT op <> (freeVarsT range <> freeVarsT term)
  freeVarsT (Case e clauses _) = freeVarsT e <> Set.unions (map (freeVarsT . snd) clauses)
  freeVarsT EHole {} = mempty

  freeVars _ = error "SCM: I don't think we need it!"

instance Free Var where
  freeVarsT (BName _) = mempty
  freeVarsT (FName x) = Set.singleton x

  freeVars _ = error "SCM: I don't think we need it!"

instance Free Chain where
  freeVarsT (Pure expr _) = freeVarsT expr
  freeVarsT (More chain _op expr _) = freeVarsT chain <> freeVarsT expr

  freeVars _ = error "SCM: I don't think we need it!"
