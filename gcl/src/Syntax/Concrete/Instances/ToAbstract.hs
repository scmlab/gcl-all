{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Syntax.Concrete.Instances.ToAbstract where

import Control.Monad.Except
  ( Except,
    throwError,
  )
import Data.Loc.Range
import qualified Hack
import Pretty.Util
  ( PrettyWithLoc (prettyWithLoc),
    docToText,
    toDoc,
  )
import qualified Syntax.Abstract as A
import qualified Syntax.Abstract.Operator as A
import Syntax.Abstract.Util
import Syntax.Common (Name (..))
import Syntax.Concrete.Types
import qualified Syntax.ConstExpr as ConstExpr

--------------------------------------------------------------------------------

-- FIXME: find some way to make this
-- class ToAbstract a b r | a -> b, a -> r where
--   toAbstract :: a -> Except r b

-- | Typeclass for converting Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> Except Range b

instance (ToAbstract a b) => ToAbstract (Maybe a) (Maybe b) where
  toAbstract Nothing = return Nothing
  toAbstract (Just x) = Just <$> toAbstract x

instance (ToAbstract a b) => ToAbstract [a] [b] where
  toAbstract = mapM toAbstract

instance ToAbstract (Name a) (Name (Maybe a)) where
  toAbstract = return . Hack.aToMaybeA

--------------------------------------------------------------------------------

-- | Program
instance (Ord a, Hack.IsRange a) => ToAbstract (Program a) (A.Program (Maybe a)) where
  toAbstract prog@(Program ds stmts') = do
    (decls, defns) <- foldl (<>) ([], []) <$> toAbstract ds

    let (globProps, assertions) = ConstExpr.pickGlobals decls
    let pre =
          [A.Assert (A.conjunct assertions) Nothing | not (null assertions)]
    stmts <- toAbstract stmts'

    return $ A.Program defns decls globProps (pre ++ stmts) (Hack.justInfo prog)

instance (Hack.IsRange a) => ToAbstract (Either (Declaration a) (DefinitionBlock a)) ([A.Declaration (Maybe a)], [A.Definition (Maybe a)]) where
  toAbstract (Left d) = do
    decls <- toAbstract d
    return ([decls], [])
  toAbstract (Right defnBlock) = do
    defns <- toAbstract defnBlock
    return ([], defns)

--------------------------------------------------------------------------------

-- | Definition
instance (Hack.IsRange a) => ToAbstract (DefinitionBlock a) [A.Definition (Maybe a)] where
  toAbstract (DefinitionBlock _ defns _) = concat <$> toAbstract defns

instance (Hack.IsRange a) => ToAbstract (Definition a) [A.Definition (Maybe a)] where
  toAbstract (TypeDefn tok name binders _ cons) = do
    (: [])
      <$> (A.TypeDefn (Hack.aToMaybeA name) (map Hack.aToMaybeA binders) <$> toAbstract cons <*> pure (Hack.rangeToA (rangeOf tok) Hack.<--> Hack.justInfo (Hack.info cons)))
  toAbstract (FuncDefnSig decl prop) = do
    (names, typ) <- toAbstract decl
    mapM
      (\n -> A.FuncDefnSig n typ <$> toAbstract prop <*> pure (Hack.info (Hack.aToMaybeA decl) Hack.<--> fmap Hack.info prop))
      names
  toAbstract (FuncDefn name args _ body) = do
    body' <- toAbstract body
    return [A.FuncDefn (Hack.aToMaybeA name) $ wrapLam (map Hack.aToMaybeA args) body']

instance (Hack.IsRange a) => ToAbstract (TypeDefnCtor a) (A.TypeDefnCtor (Maybe a)) where
  toAbstract (TypeDefnCtor c tys) = do
    tys' <- mapM toAbstract tys
    return $ A.TypeDefnCtor (Hack.aToMaybeA c) tys'

--------------------------------------------------------------------------------

-- | Declaraion
instance (Hack.IsRange a) => ToAbstract (Declaration a) (A.Declaration (Maybe a)) where
  toAbstract d = case d of
    ConstDecl _ decl -> do
      (name, body, prop) <- toAbstract decl
      return $ A.ConstDecl name body prop (Hack.justInfo d)
    VarDecl _ decl -> do
      (name, body, prop) <- toAbstract decl
      return $ A.VarDecl name body prop (Hack.justInfo d)

--------------------------------------------------------------------------------

-- | Statement
instance (Ord a, Hack.IsRange a) => ToAbstract (Stmt a) (A.Stmt (Maybe a)) where
  toAbstract stmt = case stmt of
    Skip _ -> pure (A.Skip (Hack.justInfo stmt))
    Abort _ -> pure (A.Abort (Hack.justInfo stmt))
    Assign a _ b -> do
      A.Assign <$> toAbstract a <*> toAbstract b <*> pure (Hack.justInfo stmt)
    AAssign x _ i _ _ e ->
      A.AAssign (A.Var (Hack.aToMaybeA x) (Hack.justInfo x))
        <$> toAbstract i
        <*> toAbstract e
        <*> pure
          (Hack.justInfo stmt)
    Assert _ a _ -> A.Assert <$> toAbstract a <*> pure (Hack.justInfo stmt)
    LoopInvariant _ a _ _ _ b _ ->
      A.LoopInvariant <$> toAbstract a <*> toAbstract b <*> pure (Hack.justInfo stmt)
    Do _ a _ -> A.Do <$> toAbstract a <*> pure (Hack.justInfo stmt)
    If _ a _ -> A.If <$> toAbstract a <*> pure (Hack.justInfo stmt)
    SpecQM l -> throwError $ Hack.aToRange l
    Spec l xs r -> do
      let text = docToText $ toDoc $ prettyWithLoc (map (fmap show) xs)
      pure (A.Spec text (Hack.rangeToA (rangeOf l <> rangeOf r)))
    Proof anchor contents _ r -> pure $ A.Proof anchor contents (Just r)
    Alloc p _ _ _ es _ -> A.Alloc (Hack.aToMaybeA p) <$> toAbstract es <*> pure (Hack.justInfo stmt)
    HLookup x _ _ e -> A.HLookup (Hack.aToMaybeA x) <$> toAbstract e <*> pure (Hack.justInfo stmt)
    HMutate _ e1 _ e2 ->
      A.HMutate <$> toAbstract e1 <*> toAbstract e2 <*> pure (Hack.justInfo stmt)
    Dispose _ e -> A.Dispose <$> toAbstract e <*> pure (Hack.justInfo stmt)
    Block _ p _ -> A.Block <$> toAbstract p <*> pure (Hack.justInfo stmt)

instance (Ord a, Hack.IsRange a) => ToAbstract (GdCmd a) (A.GdCmd (Maybe a)) where
  toAbstract (GdCmd a _ b) =
    A.GdCmd <$> toAbstract a <*> toAbstract b <*> pure (Hack.justInfo a Hack.<--> foldr (\x acc -> acc Hack.<--> Hack.justInfo x) Nothing b)

-- instance ToAbstract ProofAnchor A.ProofAnchor where
--   toAbstract (ProofAnchor hash range) = pure $ A.ProofAnchor hash range

-- instance ToAbstract TextContents A.TextContents where
--   toAbstract (TextContents text range) = pure $ A.TextContents text range

--------------------------------------------------------------------------------

-- Low level Declaration wrapper, and synonym types
instance (Hack.IsRange a) => ToAbstract (DeclBase a) ([Name (Maybe a)], A.Type (Maybe a)) where
  toAbstract (DeclBase a _ b) = (,) <$> toAbstract a <*> toAbstract b

instance ToAbstract (DeclProp a) (A.Expr (Maybe a)) where
  toAbstract (DeclProp _ e _) = toAbstract e

instance (Hack.IsRange a) => ToAbstract (DeclType a) ([Name (Maybe a)], A.Type (Maybe a), Maybe (A.Expr (Maybe a))) where
  toAbstract (DeclType decl prop) = do
    (ns, t) <- toAbstract decl
    e <- toAbstract prop
    return (ns, t, e)

--------------------------------------------------------------------------------

-- | Endpoint
instance ToAbstract (EndpointOpen a) (A.Endpoint (Maybe a)) where
  toAbstract (IncludingOpening _ a) = A.Including <$> toAbstract a
  toAbstract (ExcludingOpening _ a) = A.Excluding <$> toAbstract a

instance ToAbstract (EndpointClose a) (A.Endpoint (Maybe a)) where
  toAbstract (IncludingClosing a _) = A.Including <$> toAbstract a
  toAbstract (ExcludingClosing a _) = A.Excluding <$> toAbstract a

-- | Interval
instance ToAbstract (Interval a) (A.Interval (Maybe a)) where
  toAbstract i@(Interval a _ b) =
    A.Interval <$> toAbstract a <*> toAbstract b <*> pure (Hack.justInfo i)

-- | Base Type
instance ToAbstract (TBase a) A.TBase where
  toAbstract (TInt _) = pure A.TInt
  toAbstract (TBool _) = pure A.TBool
  toAbstract (TChar _) = pure A.TChar

-- | Type
-- Base types were recognized as TCon (because Base types and TCon are identical at the syntactical level),
-- and to be converted to TBase here.
instance (Hack.IsRange a) => ToAbstract (Type a) (A.Type (Maybe a)) where
  toAbstract t = case t of
    (TBase a) -> A.TBase <$> toAbstract a <*> pure (Hack.justInfo t)
    (TArray _ a _ b) ->
      A.TArray <$> toAbstract a <*> toAbstract b <*> pure (Hack.justInfo t)
    (TOp op) -> pure $ A.TOp (Hack.aToMaybeA op)
    (TData n _) -> pure $ A.TData (Hack.aToMaybeA n) (Hack.justInfo t)
    (TApp l r) -> A.TApp <$> toAbstract l <*> toAbstract r <*> pure (Hack.justInfo l Hack.<--> Hack.justInfo r)
    (TMetaVar a _) -> pure $ A.TMetaVar (Hack.aToMaybeA a) (Hack.justInfo t)
    (TParen _ a _) -> do
      t' <- toAbstract a
      case t' of
        A.TBase a' _ -> pure $ A.TBase a' (Hack.justInfo t)
        A.TArray a' b' _ -> pure $ A.TArray a' b' (Hack.justInfo t)
        A.TTuple as' -> pure $ A.TTuple as'
        A.TFunc a' b' _ -> pure $ A.TFunc a' b' (Hack.justInfo t)
        A.TOp op -> pure $ A.TOp op
        A.TData name _ -> pure $ A.TData name (Hack.justInfo t)
        A.TApp a' b' _ -> pure $ A.TApp a' b' (Hack.justInfo t)
        A.TVar a' _ -> pure $ A.TVar a' (Hack.justInfo t)
        A.TMetaVar a' _ -> pure $ A.TMetaVar a' (Hack.justInfo t)

--------------------------------------------------------------------------------

-- | Expressions
instance ToAbstract (Expr a) (A.Expr (Maybe a)) where
  toAbstract x = case x of
    Paren _ a _ -> toAbstract a
    Lit a -> A.Lit <$> toAbstract a <*> pure (Hack.justInfo x)
    Var a -> pure $ A.Var (Hack.aToMaybeA a) (Hack.justInfo x)
    Const a -> pure $ A.Const (Hack.aToMaybeA a) (Hack.justInfo x)
    Op a -> pure $ A.Op (Hack.aToMaybeA a)
    Chain ch -> A.Chain <$> toAbstract ch
    Arr arr _ i _ ->
      A.ArrIdx <$> toAbstract arr <*> toAbstract i <*> pure (Hack.justInfo x)
    App a b -> A.App <$> toAbstract a <*> toAbstract b <*> pure (Hack.justInfo x)
    Quant _ a b _ c _ d _ ->
      A.Quant
        <$> toAbstractQOp (Hack.aToMaybeA a)
        <*> pure (map Hack.aToMaybeA b)
        <*> toAbstract c
        <*> toAbstract d
        <*> pure (Hack.justInfo x)
      where
        toAbstractQOp (QuantOp' qop) = case qop of
          Left op -> return (A.Op op)
          Right n@(Name _ l) -> return $ A.Const n l
    Case _ expr _ cases ->
      A.Case <$> toAbstract expr <*> toAbstract cases <*> pure (Hack.justInfo x)

instance ToAbstract (Chain a) (A.Chain (Maybe a)) where
  toAbstract chain = case chain of
    Pure expr -> A.Pure <$> toAbstract expr <*> pure (Hack.justInfo expr)
    More ch' op expr -> A.More <$> toAbstract ch' <*> pure (Hack.aToMaybeA op) <*> toAbstract expr <*> pure (Hack.justInfo expr)

instance ToAbstract (CaseClause a) (A.CaseClause (Maybe a)) where
  toAbstract (CaseClause patt _ body) =
    A.CaseClause <$> toAbstract patt <*> toAbstract body

instance ToAbstract (Pattern a) (A.Pattern (Maybe a)) where
  toAbstract (PattLit x) = A.PattLit <$> toAbstract x
  toAbstract (PattParen _ x _) = toAbstract x
  toAbstract (PattBinder x) = return $ A.PattBinder (Hack.aToMaybeA x)
  toAbstract (PattWildcard x) = return $ A.PattWildcard (Hack.rangeToA (rangeOf x))
  toAbstract (PattConstructor ctor pats) = do
    pats' <- mapM toAbstract pats
    return $ A.PattConstructor (Hack.aToMaybeA ctor) pats'

-- | Literals (Integer / Boolean / Character)
instance ToAbstract (Lit a) (A.Lit (Maybe a)) where
  toAbstract (LitInt a _) = pure $ A.Num a
  toAbstract (LitBool a _) = pure $ A.Bol a
  toAbstract (LitChar a _) = pure $ A.Chr a

--------------------------------------------------------------------------------

instance (ToAbstract a b) => ToAbstract (SepBy sep a) [b] where
  toAbstract (Head a) = do
    b <- toAbstract a
    return [b]
  toAbstract (Delim a _ as) = do
    b <- toAbstract a
    bs <- toAbstract as
    return (b : bs)
