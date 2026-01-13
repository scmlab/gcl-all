{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Syntax.Concrete.Instances.ToAbstract where

import GCL.Range (MaybeRanged (maybeRangeOf), rangeOf, (<--->))
import Pretty.Util
  ( PrettyWithRange (prettyWithRange),
    docToText,
    toDoc,
  )
import qualified Syntax.Abstract as A
import qualified Syntax.Abstract.Operator as A
import Syntax.Abstract.Util
import Syntax.Common (Name (..))
import Syntax.Concrete.Instances.Located
  (
  )
import Syntax.Concrete.Types
import qualified Syntax.ConstExpr as ConstExpr

--------------------------------------------------------------------------------

-- | Typeclass for converting Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> b

instance (ToAbstract a b) => ToAbstract (Maybe a) (Maybe b) where
  toAbstract Nothing = Nothing
  toAbstract (Just x) = Just (toAbstract x)

instance (ToAbstract a b) => ToAbstract [a] [b] where
  toAbstract = map toAbstract

instance ToAbstract Name Name where
  toAbstract = id

--------------------------------------------------------------------------------

-- | Program
instance ToAbstract Program A.Program where
  toAbstract prog@(Program ds stmts') =
    let (decls, defns) = foldl (<>) ([], []) (toAbstract ds)
        (globProps, assertions) = ConstExpr.pickGlobals decls
        pre = [A.Assert (A.conjunct assertions) Nothing | not (null assertions)]
        stmts = toAbstract stmts'
     in A.Program defns decls globProps (pre ++ stmts) (maybeRangeOf prog)

instance ToAbstract (Either Declaration DefinitionBlock) ([A.Declaration], [A.Definition]) where
  toAbstract (Left d) =
    let decls = toAbstract d
     in ([decls], [])
  toAbstract (Right defnBlock) =
    let defns = toAbstract defnBlock
     in ([], defns)

--------------------------------------------------------------------------------

-- | Definition
instance ToAbstract DefinitionBlock [A.Definition] where
  toAbstract (DefinitionBlock _ defns _) = concat (toAbstract defns)

instance ToAbstract Definition [A.Definition] where
  toAbstract (TypeDefn tok name binders _ cons) =
    [A.TypeDefn name binders (toAbstract cons) (maybeRangeOf tok <---> maybeRangeOf cons)]
  toAbstract (FuncDefnSig decl prop) =
    let (names, typ) = toAbstract decl
        prop' = toAbstract prop
     in map (\n -> A.FuncDefnSig n typ prop' (maybeRangeOf decl <---> maybe Nothing maybeRangeOf prop)) names
  toAbstract (FuncDefn name args _ body) =
    let body' = toAbstract body
     in [A.FuncDefn name $ wrapLam args body']

instance ToAbstract TypeDefnCtor A.TypeDefnCtor where
  toAbstract (TypeDefnCtor c tys) =
    let tys' = map toAbstract tys
     in A.TypeDefnCtor c tys'

--------------------------------------------------------------------------------

-- | Declaraion
instance ToAbstract Declaration A.Declaration where
  toAbstract d = case d of
    ConstDecl _ decl ->
      let (name, body, prop) = toAbstract decl
       in A.ConstDecl name body prop (maybeRangeOf d)
    VarDecl _ decl ->
      let (name, body, prop) = toAbstract decl
       in A.VarDecl name body prop (maybeRangeOf d)

--------------------------------------------------------------------------------

-- | Statement
instance ToAbstract Stmt A.Stmt where
  toAbstract stmt = case stmt of
    Skip _ -> A.Skip (maybeRangeOf stmt)
    Abort _ -> A.Abort (maybeRangeOf stmt)
    Assign a _ b -> A.Assign (toAbstract a) (toAbstract b) (maybeRangeOf stmt)
    AAssign x _ i _ _ e ->
      A.AAssign
        (A.Var x (maybeRangeOf x))
        (toAbstract i)
        (toAbstract e)
        (maybeRangeOf stmt)
    Assert _ a _ -> A.Assert (toAbstract a) (maybeRangeOf stmt)
    LoopInvariant _ a _ _ _ b _ ->
      A.LoopInvariant (toAbstract a) (toAbstract b) (maybeRangeOf stmt)
    Do _ a _ -> A.Do (toAbstract a) (maybeRangeOf stmt)
    If _ a _ -> A.If (toAbstract a) (maybeRangeOf stmt)
    SpecQM l ->
      error $
        "BUG: toAbstract encountered SpecQM at "
          ++ show l
          ++ ". "
          ++ "Do not call toAbstract when there are holes in the concrete syntax. "
          ++ "Use collectHoles to check first, then digHoles to fill holes before calling toAbstract. "
          ++ "If you already did this, then either collectHoles missed this SpecQM, or digHoles failed to fill it."
    Spec l xs r ->
      let text = docToText $ toDoc $ prettyWithRange (map (fmap show) xs)
       in A.Spec text (rangeOf l <> rangeOf r)
    Proof anchor contents _ r -> A.Proof anchor contents r
    Alloc p _ _ _ es _ -> A.Alloc p (toAbstract es) (maybeRangeOf stmt)
    HLookup x _ _ e -> A.HLookup x (toAbstract e) (maybeRangeOf stmt)
    HMutate _ e1 _ e2 ->
      A.HMutate (toAbstract e1) (toAbstract e2) (maybeRangeOf stmt)
    Dispose _ e -> A.Dispose (toAbstract e) (maybeRangeOf stmt)
    Block _ p _ -> A.Block (toAbstract p) (maybeRangeOf stmt)

instance ToAbstract GdCmd A.GdCmd where
  toAbstract (GdCmd a _ b) =
    A.GdCmd (toAbstract a) (toAbstract b) (maybeRangeOf a <---> maybeRangeOf b)

-- instance ToAbstract ProofAnchor A.ProofAnchor where
--   toAbstract (ProofAnchor hash range) = A.ProofAnchor hash range

-- instance ToAbstract TextContents A.TextContents where
--   toAbstract (TextContents text range) = A.TextContents text range

--------------------------------------------------------------------------------

-- Low level Declaration wrapper, and synonym types
instance ToAbstract DeclBase ([Name], A.Type) where
  toAbstract (DeclBase a _ b) = (toAbstract a, toAbstract b)

instance ToAbstract DeclProp A.Expr where
  toAbstract (DeclProp _ e _) = toAbstract e

instance ToAbstract DeclType ([Name], A.Type, Maybe A.Expr) where
  toAbstract (DeclType decl prop) =
    let (ns, t) = toAbstract decl
        e = toAbstract prop
     in (ns, t, e)

--------------------------------------------------------------------------------

-- | Endpoint
instance ToAbstract EndpointOpen A.Endpoint where
  toAbstract (IncludingOpening _ a) = A.Including (toAbstract a)
  toAbstract (ExcludingOpening _ a) = A.Excluding (toAbstract a)

instance ToAbstract EndpointClose A.Endpoint where
  toAbstract (IncludingClosing a _) = A.Including (toAbstract a)
  toAbstract (ExcludingClosing a _) = A.Excluding (toAbstract a)

-- | Interval
instance ToAbstract Interval A.Interval where
  toAbstract i@(Interval a _ b) =
    A.Interval (toAbstract a) (toAbstract b) (maybeRangeOf i)

-- | Base Type
instance ToAbstract TBase A.TBase where
  toAbstract (TInt _) = A.TInt
  toAbstract (TBool _) = A.TBool
  toAbstract (TChar _) = A.TChar

-- | Type
-- Base types were recognized as TCon (because Base types and TCon are identical at the syntactical level),
-- and to be converted to TBase here.
instance ToAbstract Type A.Type where
  toAbstract t = case t of
    (TBase a) -> A.TBase (toAbstract a) (maybeRangeOf t)
    (TArray _ a _ b) ->
      A.TArray (toAbstract a) (toAbstract b) (maybeRangeOf t)
    (TOp op) -> A.TOp op
    (TData n _) -> A.TData n (maybeRangeOf t)
    (TApp l r) -> A.TApp (toAbstract l) (toAbstract r) (maybeRangeOf l <---> maybeRangeOf r)
    (TMetaVar a _) -> A.TMetaVar a (maybeRangeOf t)
    (TParen _ a _) ->
      let t' = toAbstract a
       in case t' of
            A.TBase a' _ -> A.TBase a' (maybeRangeOf t)
            A.TArray a' b' _ -> A.TArray a' b' (maybeRangeOf t)
            A.TTuple as' -> A.TTuple as'
            A.TFunc a' b' _ -> A.TFunc a' b' (maybeRangeOf t)
            A.TOp op -> A.TOp op
            A.TData name _ -> A.TData name (maybeRangeOf t)
            A.TApp a' b' _ -> A.TApp a' b' (maybeRangeOf t)
            A.TVar a' _ -> A.TVar a' (maybeRangeOf t)
            A.TMetaVar a' _ -> A.TMetaVar a' (maybeRangeOf t)

--------------------------------------------------------------------------------

-- | Expressions
instance ToAbstract Expr A.Expr where
  toAbstract x = case x of
    Paren _ a _ -> toAbstract a
    Lit a -> A.Lit (toAbstract a) (maybeRangeOf x)
    Var a -> A.Var a (maybeRangeOf x)
    Const a -> A.Const a (maybeRangeOf x)
    Op a -> A.Op a
    Chain ch -> A.Chain (toAbstract ch)
    Arr arr _ i _ ->
      A.ArrIdx (toAbstract arr) (toAbstract i) (maybeRangeOf x)
    App a b -> A.App (toAbstract a) (toAbstract b) (maybeRangeOf x)
    Quant _ a b _ c _ d _ ->
      let qop = toAbstractQOp a
       in A.Quant qop b (toAbstract c) (toAbstract d) (maybeRangeOf x)
      where
        toAbstractQOp qop = case qop of
          Left op -> A.Op op
          Right n@(Name _ l) -> A.Const n l
    Case _ expr _ cases ->
      A.Case (toAbstract expr) (toAbstract cases) (maybeRangeOf x)

instance ToAbstract Chain A.Chain where
  toAbstract chain = case chain of
    Pure expr -> A.Pure (toAbstract expr) (maybeRangeOf expr)
    More ch' op expr -> A.More (toAbstract ch') op (toAbstract expr) (maybeRangeOf expr)

instance ToAbstract CaseClause A.CaseClause where
  toAbstract (CaseClause patt _ body) =
    A.CaseClause (toAbstract patt) (toAbstract body)

instance ToAbstract Pattern A.Pattern where
  toAbstract (PattLit x) = A.PattLit (toAbstract x)
  toAbstract (PattParen _ x _) = toAbstract x
  toAbstract (PattBinder x) = A.PattBinder x
  toAbstract (PattWildcard x) = A.PattWildcard (rangeOf x)
  toAbstract (PattConstructor ctor pats) =
    let pats' = map toAbstract pats
     in A.PattConstructor ctor pats'

-- | Literals (Integer / Boolean / Character)
instance ToAbstract Lit A.Lit where
  toAbstract (LitInt a _) = A.Num a
  toAbstract (LitBool a _) = A.Bol a
  toAbstract (LitChar a _) = A.Chr a

--------------------------------------------------------------------------------

instance (ToAbstract a b) => ToAbstract (SepBy sep a) [b] where
  toAbstract (Head a) =
    let b = toAbstract a
     in [b]
  toAbstract (Delim a _ as) =
    let b = toAbstract a
        bs = toAbstract as
     in (b : bs)
