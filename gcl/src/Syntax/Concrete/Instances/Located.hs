module Syntax.Concrete.Instances.Located where

import Data.Loc.Range
  ( MaybeRanged (..),
    (<->>),
  )
import Syntax.Common ()
import Syntax.Concrete.Types

--------------------------------------------------------------------------------

instance (MaybeRanged a) => MaybeRanged (SepBy sep a) where
  maybeRangeOf (Head a) = maybeRangeOf a
  maybeRangeOf (Delim a _ as) = maybeRangeOf a <->> maybeRangeOf as

instance (MaybeRanged a, MaybeRanged c) => MaybeRanged (a, b, c) where
  maybeRangeOf (a, _, c) = maybeRangeOf a <->> maybeRangeOf c

--------------------------------------------------------------------------------

instance MaybeRanged Program where
  maybeRangeOf (Program a b) = maybeRangeOf a <->> maybeRangeOf b

instance MaybeRanged Declaration where
  maybeRangeOf (ConstDecl l r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (VarDecl l r) = maybeRangeOf l <->> maybeRangeOf r

instance MaybeRanged TypeDefnCtor where
  maybeRangeOf (TypeDefnCtor l r) = maybeRangeOf l <->> maybeRangeOf r

instance MaybeRanged DefinitionBlock where
  maybeRangeOf (DefinitionBlock l _ r) = maybeRangeOf l <->> maybeRangeOf r

instance MaybeRanged DeclBase where
  maybeRangeOf (DeclBase l _ r) = maybeRangeOf l <->> maybeRangeOf r

instance MaybeRanged DeclProp where
  maybeRangeOf (DeclProp l _ r) = maybeRangeOf l <->> maybeRangeOf r

instance MaybeRanged DeclType where
  maybeRangeOf (DeclType l r) = maybeRangeOf l <->> maybe Nothing maybeRangeOf r

-------------------------------------------------------------------------------

instance MaybeRanged Stmt where
  maybeRangeOf (Skip l) = Just l
  maybeRangeOf (Abort l) = Just l
  maybeRangeOf (Assign l _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (AAssign l _ _ _ _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (Assert l _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (LoopInvariant l _ _ _ _ _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (Do l _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (If l _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (SpecQM l) = Just l
  maybeRangeOf (Spec l _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (Proof _ _ _ r) = Just r
  maybeRangeOf (Alloc l _ _ _ _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (HLookup l _ _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (HMutate l _ _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (Dispose l r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (Block l _ r) = maybeRangeOf l <->> maybeRangeOf r

--------------------------------------------------------------------------------

instance MaybeRanged EndpointOpen where
  maybeRangeOf (IncludingOpening l e) = maybeRangeOf l <->> maybeRangeOf e
  maybeRangeOf (ExcludingOpening l e) = maybeRangeOf l <->> maybeRangeOf e

instance MaybeRanged EndpointClose where
  maybeRangeOf (IncludingClosing e l) = maybeRangeOf e <->> maybeRangeOf l
  maybeRangeOf (ExcludingClosing e l) = maybeRangeOf e <->> maybeRangeOf l

instance MaybeRanged Interval where
  maybeRangeOf (Interval l _ r) = maybeRangeOf l <->> maybeRangeOf r

instance MaybeRanged TBase where
  maybeRangeOf (TInt l) = Just l
  maybeRangeOf (TBool l) = Just l
  maybeRangeOf (TChar l) = Just l

instance MaybeRanged Type where
  maybeRangeOf (TParen l _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (TBase a) = maybeRangeOf a
  maybeRangeOf (TArray l _ _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (TOp op) = maybeRangeOf op
  maybeRangeOf (TData _ l) = Just l
  maybeRangeOf (TApp l r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (TMetaVar _ l) = Just l

--------------------------------------------------------------------------------

instance MaybeRanged Expr where
  maybeRangeOf (Paren l _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (Lit x) = maybeRangeOf x
  maybeRangeOf (Var x) = maybeRangeOf x
  maybeRangeOf (Const x) = maybeRangeOf x
  maybeRangeOf (Op x) = maybeRangeOf x
  maybeRangeOf (Chain ch) = maybeRangeOf ch
  maybeRangeOf (Arr l _ _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (App x y) = maybeRangeOf x <->> maybeRangeOf y
  maybeRangeOf (Quant l _ _ _ _ _ _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (Case l _ _ r) = maybeRangeOf l <->> maybeRangeOf r

instance MaybeRanged Chain where
  maybeRangeOf (Pure expr) = maybeRangeOf expr
  maybeRangeOf (More ch _ expr) = maybeRangeOf ch <->> maybeRangeOf expr

instance MaybeRanged CaseClause where
  maybeRangeOf (CaseClause l _ r) = maybeRangeOf l <->> maybeRangeOf r

--------------------------------------------------------------------------------

instance MaybeRanged Pattern where
  maybeRangeOf (PattLit l) = maybeRangeOf l
  maybeRangeOf (PattParen l _ r) = maybeRangeOf l <->> maybeRangeOf r
  maybeRangeOf (PattBinder l) = maybeRangeOf l
  maybeRangeOf (PattWildcard l) = maybeRangeOf l
  maybeRangeOf (PattConstructor l r) = maybeRangeOf l <->> maybeRangeOf r

--------------------------------------------------------------------------------

instance MaybeRanged Lit where
  maybeRangeOf (LitInt _ l) = Just l
  maybeRangeOf (LitBool _ l) = Just l
  maybeRangeOf (LitChar _ l) = Just l
