module Syntax.AbstractLN.Instances.Located where

import GCL.Range (MaybeRanged (..), (<--->))
import Syntax.AbstractLN.Types
import Syntax.Common ()
import Prelude hiding (Ordering (..))

-- MaybeRanged instances (primary)
-- instance MaybeRanged Program where
--   maybeRangeOf (Program _ _ _ _ _ l) = l
--
-- instance MaybeRanged Declaration where
--   maybeRangeOf (ConstDecl _ _ _ l) = l
--   maybeRangeOf (VarDecl _ _ _ l) = l
--
-- instance MaybeRanged Definition where
--   maybeRangeOf (TypeDefn _ _ _ r) = r
--   maybeRangeOf (ValDefn l m rs) =
--     maybeRangeOf l <---> maybeRangeOf m <---> maybeRangeOf rs
--
-- instance MaybeRanged TypeDefnCtor where
--   maybeRangeOf (TypeDefnCtor l r) = maybeRangeOf l <---> maybeRangeOf r

instance MaybeRanged Stmt where
  maybeRangeOf (Skip l) = l
  maybeRangeOf (Abort l) = l
  maybeRangeOf (Assign _ l) = l
  maybeRangeOf (AAssign _ _ _ l) = l
  maybeRangeOf (Assert _ l) = l
  maybeRangeOf (LoopInvariant _ _ l) = l
  maybeRangeOf (Do _ l) = l
  maybeRangeOf (If _ l) = l
  maybeRangeOf (Spec _ l) = Just l
  maybeRangeOf (Alloc _ _ l) = l
  maybeRangeOf (HLookup _ _ l) = l
  maybeRangeOf (HMutate _ _ l) = l
  maybeRangeOf (Dispose _ l) = l

--  maybeRangeOf (Block _ l) = l

instance MaybeRanged GdCmd where
  maybeRangeOf (GdCmd _ _ l) = l

instance MaybeRanged Type where
  maybeRangeOf (TLit _ l) = l
  maybeRangeOf (TVar _ l) = l
  maybeRangeOf (TApp _ _ l) = l
  maybeRangeOf (TForall _ _ l) = l
  maybeRangeOf TType = Nothing

  justSetRange l (TLit t _) = TLit t (Just l)
  justSetRange l (TVar v _) = TVar v (Just l)
  justSetRange l (TApp t0 t1 _) = TApp t0 t1 (Just l)
  justSetRange l (TForall xs e _) = TForall xs e (Just l)
  justSetRange l TType = TType

-- instance MaybeRanged Kind where
--   maybeRangeOf (KStar loc) = loc
--   maybeRangeOf (KFunc _ _ loc) = loc
--   maybeRangeOf (KMetaVar _) = Nothing

instance MaybeRanged Expr where
  maybeRangeOf (Lit _ l) = l
  maybeRangeOf (Var _ l) = l
  maybeRangeOf (Chain chain) = maybeRangeOf chain
  maybeRangeOf (App _ _ l) = l
  maybeRangeOf (Lam _ _ l) = l
  maybeRangeOf (Quant _ _ _ _ l) = l
  maybeRangeOf (Case _ _ l) = l
  maybeRangeOf (EHole h) = maybeRangeOf h

  justSetRange l (Lit t _) = Lit t (Just l)
  justSetRange l (Var v _) = Var v (Just l)
  justSetRange l (Chain chain) = Chain (justSetRange l chain)
  justSetRange l (App e0 e1 _) = App e0 e1 (Just l)
  justSetRange l (Lam x e _) = Lam x e (Just l)
  justSetRange l (Quant op xs ran trm _) = Quant op xs ran trm (Just l)
  justSetRange l (Case m cls _) = Case m cls (Just l)
  justSetRange l (EHole h) = EHole (justSetRange l h)

-- instance MaybeRanged Var where
--   maybeRangeOf (FName _ l) = l
--   maybeRangeOf (BName _ l) = l

instance MaybeRanged Chain where
  maybeRangeOf (Pure _ l) = l
  maybeRangeOf (More _ _ _ l) = l

  justSetRange l (Pure x _) = Pure x (Just l)
  justSetRange l (More ch op x _) = More ch op x (Just l)

instance MaybeRanged Pattern where
  maybeRangeOf (PattBinder _ l) = l
  maybeRangeOf (PattWildcard l) = Just l
  maybeRangeOf (PattConstructor _ l r) = l <---> maybeRangeOf r

instance MaybeRanged Hole where
  maybeRangeOf (Hole _ _ l) = Just l
  justSetRange l (Hole txt no _) = Hole txt no l
