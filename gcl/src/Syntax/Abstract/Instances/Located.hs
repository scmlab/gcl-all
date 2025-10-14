module Syntax.Abstract.Instances.Located where

import Data.Loc (Loc (NoLoc), Located (locOf), (<-->))
import Syntax.Abstract.Types
import Syntax.Common ()
import Prelude hiding (Ordering (..))

instance (Located a) => Located (Program a) where
  locOf (Program _ _ _ _ l) = locOf l

instance (Located a) => Located (Declaration a) where
  locOf (ConstDecl _ _ _ l) = locOf l
  locOf (VarDecl _ _ _ l) = locOf l

instance (Located a) => Located (Definition a) where
  locOf (TypeDefn _ _ _ r) = locOf r
  locOf (FuncDefnSig _ _ _ r) = locOf r
  locOf (FuncDefn l r) = l <--> r

instance (Located a) => Located (TypeDefnCtor a) where
  locOf (TypeDefnCtor l r) = l <--> r

instance (Located a) => Located (Stmt a) where
  locOf (Skip l) = locOf l
  locOf (Abort l) = locOf l
  locOf (Assign _ _ l) = locOf l
  locOf (AAssign _ _ _ l) = locOf l
  locOf (Assert _ l) = locOf l
  locOf (LoopInvariant _ _ l) = locOf l
  locOf (Do _ l) = locOf l
  locOf (If _ l) = locOf l
  locOf (Spec _ l) = locOf l
  locOf (Proof _ _ r) = locOf r
  locOf (Alloc _ _ l) = locOf l
  locOf (HLookup _ _ l) = locOf l
  locOf (HMutate _ _ l) = locOf l
  locOf (Dispose _ l) = locOf l
  locOf (Block _ l) = locOf l

instance (Located a) => Located (GdCmd a) where
  locOf (GdCmd _ _ l) = locOf l

instance (Located a) => Located (Endpoint a) where
  locOf (Including e) = locOf e
  locOf (Excluding e) = locOf e

instance (Located a) => Located (Interval a) where
  locOf (Interval _ _ l) = locOf l

instance (Located a) => Located (Type a) where
  locOf (TBase _ l) = locOf l
  locOf (TArray _ _ l) = locOf l
  locOf (TTuple _) = NoLoc
  locOf (TFunc _ _ l) = locOf l
  locOf (TOp op) = locOf op
  locOf (TData _ l) = locOf l
  locOf (TApp _ _ l) = locOf l
  locOf (TVar _ l) = locOf l
  locOf (TMetaVar _ l) = locOf l

instance (Located a) => Located (Kind a) where
  locOf (KStar l) = locOf l
  locOf (KFunc _ _ l) = locOf l
  locOf (KMetaVar _) = NoLoc

instance (Located a) => Located (Expr a) where
  locOf (Var _ l) = locOf l
  locOf (Const _ l) = locOf l
  locOf (Lit _ l) = locOf l
  locOf (Op op) = locOf op
  locOf (Chain chain) = locOf chain
  locOf (App _ _ l) = locOf l
  locOf (Func _ _ l) = locOf l
  locOf (Lam _ _ l) = locOf l
  locOf (Tuple _) = NoLoc
  locOf (Quant _ _ _ _ l) = locOf l
  locOf (RedexKernel es _ _ _) = locOf es
  locOf (RedexShell _ x) = locOf x
  locOf (ArrIdx _ _ l) = locOf l
  locOf (ArrUpd _ _ _ l) = locOf l
  locOf (Case _ _ l) = locOf l

instance (Located a) => Located (Chain a) where
  locOf (Pure _ l) = locOf l
  locOf (More _ _ _ l) = locOf l

instance (Located a) => Located (CaseClause a) where
  locOf (CaseClause l r) = l <--> r

instance (Located a) => Located (Pattern a) where
  locOf (PattLit l) = locOf l
  locOf (PattBinder l) = locOf l
  locOf (PattWildcard l) = locOf l
  locOf (PattConstructor l r) = l <--> r

instance Located (Lit a) where
  locOf _ = NoLoc
