module Syntax.Concrete.Instances.Free where

import Data.Set ( Set, (\\) )
import qualified Data.Set as Set
import GCL.Common (Free (..))
import Syntax.Concrete.Types

import Syntax.Common (ArithOp, Name (..), nameToText)

import GCL.Common(Free (..))

instance Free Expr where
  freeVarsT (Paren _ e _) = freeVarsT e
  freeVarsT (Lit _) = mempty
  freeVarsT (Var x) = Set.singleton (nameToText x)
  freeVarsT (Const x) = Set.singleton (nameToText x)
  freeVarsT (Op _) = mempty
  freeVarsT (Chain chain) = freeVarsT chain
  freeVarsT (Arr arr _ idx _) = freeVarsT arr <> freeVarsT idx
  freeVarsT (App e1 e2) = freeVarsT e1 <> freeVarsT e2
  freeVarsT (Quant _ _op xs _ range _ term _) =
    (freeVarsT range <> freeVarsT term) \\ Set.fromList (map nameToText xs)
  freeVarsT (Case _ e _ clauses) = freeVarsT e <> Set.unions (map freeVarsT clauses)
  freeVarsT EHole {} = mempty

  freeVars _ = error "SCM: I don't think we need it!"

instance Free Chain where
  freeVarsT (Pure expr) = freeVarsT expr
  freeVarsT (More chain _op expr) = freeVarsT chain <> freeVarsT expr

  freeVars _ = error "SCM: I don't think we need it!"

instance Free CaseClause where
  freeVarsT (CaseClause patt _ expr) = freeVarsT expr \\ freeVarsT patt

  freeVars _ = error "SCM: I don't think we need it!"

instance Free Pattern where
  freeVarsT (PattLit _) = mempty
  freeVarsT (PattParen _ p _) = freeVarsT p
  freeVarsT (PattBinder n) = Set.singleton (nameToText n)
  freeVarsT (PattWildcard _) = mempty
  freeVarsT (PattConstructor _ ps) = foldMap freeVarsT ps

  freeVars _ = error "SCM: I don't think we need it!"
