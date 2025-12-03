module Syntax.Abstract.Instances.Located where

import Data.Loc (Loc(..), Located(..), (<-->))
import Data.Loc.Range (Range, MaybeRanged(..), maybeRangeToLoc, (<->>))
import Syntax.Abstract.Types
import Syntax.Common ()
import Prelude hiding (Ordering (..))

-- Helper to convert Maybe Range to Loc for backward compatibility
mrToLoc :: Maybe Range -> Loc
mrToLoc = maybeRangeToLoc

-- MaybeRanged instances (primary)
instance MaybeRanged Program where
  maybeRangeOf (Program _ _ _ _ l) = l

instance MaybeRanged Declaration where
  maybeRangeOf (ConstDecl _ _ _ l) = l
  maybeRangeOf (VarDecl _ _ _ l) = l

instance MaybeRanged Definition where
  maybeRangeOf (TypeDefn _ _ _ r) = r
  maybeRangeOf (FuncDefnSig _ _ _ r) = r
  maybeRangeOf (FuncDefn l r) = maybeRangeOf l <->> maybeRangeOf r

instance MaybeRanged TypeDefnCtor where
  maybeRangeOf (TypeDefnCtor l r) = maybeRangeOf l <->> maybeRangeOf r

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
  maybeRangeOf (CaseClause l r) = maybeRangeOf l <->> maybeRangeOf r

instance MaybeRanged Pattern where
  maybeRangeOf (PattLit l) = maybeRangeOf l
  maybeRangeOf (PattBinder l) = maybeRangeOf l
  maybeRangeOf (PattWildcard l) = Just l
  maybeRangeOf (PattConstructor l r) = maybeRangeOf l <->> maybeRangeOf r

instance MaybeRanged Lit where
  maybeRangeOf _ = Nothing

-- Backward compatibility: Located instances (convert Maybe Range to Loc)
instance Located Program where
  locOf (Program _ _ _ _ l) = mrToLoc l

instance Located Declaration where
  locOf (ConstDecl _ _ _ l) = mrToLoc l
  locOf (VarDecl _ _ _ l) = mrToLoc l

instance Located Definition where
  locOf (TypeDefn _ _ _ r) = mrToLoc r
  locOf (FuncDefnSig _ _ _ r) = mrToLoc r
  locOf (FuncDefn l r) = locOf l <--> locOf r

instance Located TypeDefnCtor where
  locOf (TypeDefnCtor l r) = locOf l <--> locOf r

instance Located Stmt where
  locOf (Skip l) = mrToLoc l
  locOf (Abort l) = mrToLoc l
  locOf (Assign _ _ l) = mrToLoc l
  locOf (AAssign _ _ _ l) = mrToLoc l
  locOf (Assert _ l) = mrToLoc l
  locOf (LoopInvariant _ _ l) = mrToLoc l
  locOf (Do _ l) = mrToLoc l
  locOf (If _ l) = mrToLoc l
  locOf (Spec _ l) = locOf l
  locOf (Proof _ _ r) = locOf r
  locOf (Alloc _ _ l) = mrToLoc l
  locOf (HLookup _ _ l) = mrToLoc l
  locOf (HMutate _ _ l) = mrToLoc l
  locOf (Dispose _ l) = mrToLoc l
  locOf (Block _ l) = mrToLoc l

instance Located GdCmd where
  locOf (GdCmd _ _ l) = mrToLoc l

instance Located Endpoint where
  locOf (Including e) = locOf e
  locOf (Excluding e) = locOf e

instance Located Interval where
  locOf (Interval _ _ l) = mrToLoc l

instance Located Type where
  locOf (TBase _ l) = mrToLoc l
  locOf (TArray _ _ l) = mrToLoc l
  locOf (TTuple _) = NoLoc
  locOf (TFunc _ _ l) = mrToLoc l
  locOf (TOp op) = locOf op
  locOf (TData _ l) = mrToLoc l
  locOf (TApp _ _ l) = mrToLoc l
  locOf (TVar _ l) = mrToLoc l
  locOf (TMetaVar _ l) = mrToLoc l

instance Located Kind where
  locOf (KStar loc) = mrToLoc loc
  locOf (KFunc _ _ loc) = mrToLoc loc
  locOf (KMetaVar _) = NoLoc

instance Located Expr where
  locOf (Var _ l) = mrToLoc l
  locOf (Const _ l) = mrToLoc l
  locOf (Lit _ l) = mrToLoc l
  locOf (Op op) = locOf op
  locOf (Chain chain) = locOf chain
  locOf (App _ _ l) = mrToLoc l
  locOf (Func _ _ l) = mrToLoc l
  locOf (Lam _ _ l) = mrToLoc l
  locOf (Tuple _) = NoLoc
  locOf (Quant _ _ _ _ l) = mrToLoc l
  locOf (RedexKernel es _ _ _) = locOf es
  locOf (RedexShell _ x) = locOf x
  locOf (ArrIdx _ _ l) = mrToLoc l
  locOf (ArrUpd _ _ _ l) = mrToLoc l
  locOf (Case _ _ l) = mrToLoc l

instance Located Chain where
  locOf (Pure _ l) = mrToLoc l
  locOf (More _ _ _ l) = mrToLoc l

instance Located CaseClause where
  locOf (CaseClause l r) = locOf l <--> locOf r

instance Located Pattern where
  locOf (PattLit l) = locOf l
  locOf (PattBinder l) = locOf l
  locOf (PattWildcard l) = locOf l
  locOf (PattConstructor l r) = locOf l <--> locOf r

instance Located Lit where
  locOf _ = NoLoc
