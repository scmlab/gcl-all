module Syntax.Typed.Instances.Located where

import GCL.Range (MaybeRanged (..), (<--->))
import Syntax.Common ()
import Syntax.Typed.Types
import Prelude hiding (Ordering (..))

instance MaybeRanged Program where
  maybeRangeOf (Program _ _ _ _ l) = l

instance MaybeRanged Declaration where
  maybeRangeOf (ConstDecl _ _ _ l) = l
  maybeRangeOf (VarDecl _ _ _ l) = l

instance MaybeRanged Definition where
  maybeRangeOf (TypeDefn _ _ _ r) = r
  maybeRangeOf (FuncDefnSig _ _ _ r) = r
  maybeRangeOf (FuncDefn l r) = maybeRangeOf l <---> maybeRangeOf r

instance MaybeRanged TypeDefnCtor where
  maybeRangeOf (TypeDefnCtor l r) = maybeRangeOf l <---> maybeRangeOf r

instance MaybeRanged Stmt where
  maybeRangeOf (Skip l) = l
  maybeRangeOf (Abort l) = l
  maybeRangeOf (Assign _ _ l) = l
  maybeRangeOf (AAssign _ _ _ l) = l
  maybeRangeOf (Assert _ l) = l
  maybeRangeOf (LoopInvariant _ _ l) = l
  maybeRangeOf (Do _ l) = l
  maybeRangeOf (If _ l) = l
  maybeRangeOf (Spec _ r _) = Just r
  maybeRangeOf (Proof _ _ r) = Just r
  maybeRangeOf (Alloc _ _ l) = l
  maybeRangeOf (HLookup _ _ l) = l
  maybeRangeOf (HMutate _ _ l) = l
  maybeRangeOf (Dispose _ l) = l
  maybeRangeOf (Block _ l) = l

instance MaybeRanged GdCmd where
  maybeRangeOf (GdCmd _ _ l) = l

instance MaybeRanged Expr where
  maybeRangeOf (Lit _ _ l) = l
  maybeRangeOf (Var _ _ l) = l
  maybeRangeOf (Const _ _ l) = l
  maybeRangeOf (Op op _) = maybeRangeOf op
  maybeRangeOf (Chain chain) = maybeRangeOf chain
  maybeRangeOf (App _ _ l) = l
  maybeRangeOf (Lam _ _ _ l) = l
  maybeRangeOf (Quant _ _ _ _ l) = l
  maybeRangeOf (ArrIdx _ _ l) = l
  maybeRangeOf (ArrUpd _ _ _ l) = l
  maybeRangeOf (Case _ _ l) = l
  maybeRangeOf (Subst _ _) = Nothing

instance MaybeRanged Chain where
  maybeRangeOf (Pure e) = maybeRangeOf e
  maybeRangeOf (More _ _ _ e) = maybeRangeOf e
