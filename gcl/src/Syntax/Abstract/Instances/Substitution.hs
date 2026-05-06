{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax.Abstract.Instances.Substitution where

import Control.Monad (forM)
import Data.Map hiding (map)
import qualified Data.Map as Map
import GCL.Common
import Syntax.Abstract.Types
import Syntax.Abstract.Util (declaredNames)
import Syntax.Common
import Syntax.Substitution
import Prelude hiding (Ordering (..), lookup)

instance Variableous Expr () where
  isVar (Var x _) = Just (x, ())
  isVar (Const x _) = Just (x, ())
  isVar _ = Nothing
  mkVar x () l = Var x l

instance (Fresh m) => Substitutable m Expr Expr where
  subst _ e@(Lit _ _) = return e
  subst sb (Var x l) =
    return . maybe (Var x l) id $ lookup (nameToText x) sb
  subst sb (Const x l) =
    return . maybe (Var x l) id $ lookup (nameToText x) sb
  subst _ e@(Op _) = return e
  subst sb (Chain ch) = Chain <$> subst sb ch
  subst sb (App e1 e2 l) =
    App <$> subst sb e1 <*> subst sb e2 <*> pure l
  subst sb (Lam x e l)
    | nameToText x `elem` keys sb = return (Lam x e l)
    | otherwise = do
        (xs', e', _) <- substBinderTypeless sb [x] e
        return (Lam (head xs') e' l)
  subst sb (Tuple es) = Tuple <$> mapM (subst sb) es
  subst sb (OutT i e) = OutT i <$> subst sb e
  subst sb (Quant op xs ran body l) = do
    (xs', (ran', body'), _) <- substBinderTypeless sb xs (ran, body)
    return $ Quant op xs' ran' body' l
  subst _ (RedexKernel _ _ _ _) = error "not knowing what is going on here"
  subst _ (RedexShell _ _) = error "not knowing what is going on here"
  subst sb (ArrIdx a i l) =
    ArrIdx <$> subst sb a <*> subst sb i <*> pure l
  subst sb (ArrUpd a i v l) =
    ArrUpd <$> subst sb a <*> subst sb i <*> subst sb v <*> pure l
  subst sb (Case e cases l) = do
    cases' <- forM cases $
      \(CaseClause patt body) -> CaseClause patt <$> subst sb body
    return $ Case e cases' l
  -- ChAoS: Is this the correct behavior for specification?
  subst _ e@EHole {} = return e

-- just a wrapper calling substBinder, when e is typeless Expr
substBinderTypeless ::
  (Fresh m, Substitutable m a Expr, Free a) =>
  Subst Expr ->
  [Name] ->
  a ->
  m ([Name], a, Subst Expr)
substBinderTypeless sb binders body =
  (\(binders', body', sb') -> (map fst binders', body', sb'))
    <$> (substBinder sb [(b, ()) | b <- binders] body)

instance (Fresh m) => Substitutable m Chain Expr where
  subst sb (Pure expr loc) = Pure <$> subst sb expr <*> pure loc
  subst sb (More ch' op expr loc) = More <$> subst sb ch' <*> pure op <*> subst sb expr <*> pure loc

-- instance (Fresh m) => Substitutable m FuncClause Expr where
--   subst _ = return

-- SCM: deal with this later.

instance (Fresh m) => Substitutable m Stmt Expr where
  subst _ s@(Skip _) = return s
  subst _ s@(Abort _) = return s
  subst sb (Assign ns es l) = do
    let ns' = renameVars sb ns --- this could fail!
    Assign ns' <$> subst sb es <*> pure l
  subst sb (AAssign a i v l) =
    AAssign <$> subst sb a <*> subst sb i <*> subst sb v <*> pure l
  subst sb (Assert e l) = Assert <$> subst sb e <*> pure l
  subst sb (LoopInvariant e b l) =
    LoopInvariant <$> subst sb e <*> subst sb b <*> pure l
  subst sb (Do gdcmds l) =
    Do <$> mapM (subst sb) gdcmds <*> pure l
  subst sb (If gdcmds l) =
    If <$> mapM (subst sb) gdcmds <*> pure l
  subst _ s@(Spec _ _) = return s
  subst _ s@(Proof _ _ _) = return s
  subst sb (Alloc x es l) =
    Alloc (renameVar sb x) <$> mapM (subst sb) es <*> pure l
  subst sb (HLookup x i l) =
    HLookup (renameVar sb x) <$> subst sb i <*> pure l
  subst sb (HMutate e v l) =
    HMutate <$> subst sb e <*> subst sb v <*> pure l
  subst sb (Dispose e l) =
    Dispose <$> subst sb e <*> pure l
  subst sb (Block prog l) = Block <$> subst sb prog <*> pure l

instance (Fresh m) => Substitutable m GdCmd Expr where
  subst sb (GdCmd e stmts l) =
    GdCmd
      <$> subst sb e
      <*> mapM (subst sb) stmts
      <*> pure l

instance (Fresh m) => Substitutable m Program Expr where
  subst sb (Program defns decls props stmts l) = do
    (_, (decls', props', stmts'), _) <-
      substBinderTypeless sb locals (decls, props, stmts)
    return $ Program defns decls' props' stmts' l
    where
      -- SCM: TODO: deal with defns
      locals = declaredNames decls

instance (Fresh m) => Substitutable m Declaration Expr where
  subst sb (ConstDecl ns t expr l) =
    ConstDecl (renameVars sb ns) t <$> subst sb expr <*> pure l
  subst sb (VarDecl ns t expr l) =
    VarDecl (renameVars sb ns) t <$> subst sb expr <*> pure l
