module Syntax.Abstract.Instances.Located where

import Data.Loc.Range (MaybeRanged (..), (<--->))
import Syntax.Abstract.Types
import Syntax.Common ()
import Prelude hiding (Ordering (..))

-- MaybeRanged instances (primary)
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
  maybeRangeOf (Spec _ l) = Just l
  maybeRangeOf (Proof _ _ r) = Just r
  maybeRangeOf (Alloc _ _ l) = l
  maybeRangeOf (HLookup _ _ l) = l
  maybeRangeOf (HMutate _ _ l) = l
  maybeRangeOf (Dispose _ l) = l
  maybeRangeOf (Block _ l) = l

instance MaybeRanged GdCmd where
  maybeRangeOf (GdCmd _ _ l) = l

instance MaybeRanged Endpoint where
  maybeRangeOf (Including e) = maybeRangeOf e
  maybeRangeOf (Excluding e) = maybeRangeOf e

instance MaybeRanged Interval where
  maybeRangeOf (Interval _ _ l) = l

instance MaybeRanged Type where
  maybeRangeOf (TBase _ l) = l
  maybeRangeOf (TArray _ _ l) = l
  maybeRangeOf (TTuple _) = Nothing
  maybeRangeOf (TFunc _ _ l) = l
  maybeRangeOf (TOp op) = maybeRangeOf op
  maybeRangeOf (TData _ l) = l
  maybeRangeOf (TApp _ _ l) = l
  maybeRangeOf (TVar _ l) = l
  maybeRangeOf (TMetaVar _ l) = l

instance MaybeRanged Kind where
  maybeRangeOf (KStar loc) = loc
  maybeRangeOf (KFunc _ _ loc) = loc
  maybeRangeOf (KMetaVar _) = Nothing

instance MaybeRanged Expr where
  maybeRangeOf (Var _ l) = l
  maybeRangeOf (Const _ l) = l
  maybeRangeOf (Lit _ l) = l
  maybeRangeOf (Op op) = maybeRangeOf op
  maybeRangeOf (Chain chain) = maybeRangeOf chain
  maybeRangeOf (App _ _ l) = l
  maybeRangeOf (Func _ _ l) = l
  maybeRangeOf (Lam _ _ l) = l
  maybeRangeOf (Tuple _) = Nothing
  maybeRangeOf (Quant _ _ _ _ l) = l
  maybeRangeOf (RedexKernel es _ _ _) = maybeRangeOf es
  maybeRangeOf (RedexShell _ x) = maybeRangeOf x
  maybeRangeOf (ArrIdx _ _ l) = l
  maybeRangeOf (ArrUpd _ _ _ l) = l
  maybeRangeOf (Case _ _ l) = l

instance MaybeRanged Chain where
  maybeRangeOf (Pure _ l) = l
  maybeRangeOf (More _ _ _ l) = l

instance MaybeRanged CaseClause where
  maybeRangeOf (CaseClause l r) = maybeRangeOf l <---> maybeRangeOf r

instance MaybeRanged Pattern where
  maybeRangeOf (PattLit l) = maybeRangeOf l
  maybeRangeOf (PattBinder l) = maybeRangeOf l
  maybeRangeOf (PattWildcard l) = Just l
  maybeRangeOf (PattConstructor l r) = maybeRangeOf l <---> maybeRangeOf r

instance MaybeRanged Lit where
  maybeRangeOf _ = Nothing
