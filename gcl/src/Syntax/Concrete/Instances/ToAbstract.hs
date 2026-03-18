{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Syntax.Concrete.Instances.ToAbstract where

import Control.Arrow ((***))
import Control.Monad.State
import qualified Data.Text as Text
import GCL.Common
import GCL.Range (MaybeRanged (maybeRangeOf), Range (..), rangeOf, (<--->))
import Pretty.Util
  ( PrettyWithRange (prettyWithRange),
    docToText,
    toDoc,
  )
import qualified Syntax.Abstract as A
import qualified Syntax.Abstract.Operator as A
import Syntax.Abstract.Util
import Syntax.Common (ArithOp, Name (..))
import Syntax.Concrete.Instances.Located ()
import Syntax.Concrete.Types
import qualified Syntax.ConstExpr as ConstExpr

--------------------------------------------------------------------------------

type HoleCount = Int

-- | Abstract Syntax Transformer Monad
type AbstractTransformerM = State HoleCount

runAbstractTransform :: (ToAbstract a b) => a -> b
runAbstractTransform a = evalState (toAbstract a) 0

-- | Typeclass for converting Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> AbsM b

type AbsM a = State Int a

instance Counterous (State Int) where
  countUp = do
    c <- get
    put (c + 1)
    return c

instance (ToAbstract a b) => ToAbstract (Maybe a) (Maybe b) where
  toAbstract Nothing = return Nothing
  toAbstract (Just x) = Just <$> toAbstract x

instance (ToAbstract a b) => ToAbstract [a] [b] where
  toAbstract = mapM toAbstract

instance ToAbstract Name Name where
  toAbstract = return

--------------------------------------------------------------------------------

-- | Program
instance ToAbstract Program A.Program where
  toAbstract prog@(Program ds stmts') = do
    (decls, defns) <- foldl (<>) ([], []) <$> (mapM toAbstract ds)
    let (globProps, assertions) = ConstExpr.pickGlobals decls
    let pre = [A.Assert (A.conjunct assertions) Nothing | not (null assertions)]
    stmts <- toAbstract stmts'
    return $ A.Program defns decls globProps (pre ++ stmts) (maybeRangeOf prog)

instance ToAbstract (Either Declaration DefinitionBlock) ([A.Declaration], [A.Definition]) where
  toAbstract (Left d) = (\d' -> ([d'], [])) <$> toAbstract d
  toAbstract (Right defnBlock) = (\ds -> ([], ds)) <$> toAbstract defnBlock

--------------------------------------------------------------------------------

-- | Definition
instance ToAbstract DefinitionBlock [A.Definition] where
  toAbstract (DefinitionBlock _ defns _) = do
    let (tdefs, vdefs) = splitTypeValDefns defns
    (++) <$> mapM tdefToAbs tdefs <*> vdefsToAbs vdefs

splitTypeValDefns :: [Definition] -> ([Definition], [Definition])
splitTypeValDefns [] = ([], [])
splitTypeValDefns (d@(TypeDefn {}) : ds) = ((d :) *** id) (splitTypeValDefns ds)
splitTypeValDefns (d@(ValDefnSig {}) : ds) = (id *** (d :)) (splitTypeValDefns ds)
splitTypeValDefns (d@(ValDefn {}) : ds) = (id *** (d :)) (splitTypeValDefns ds)

tdefToAbs :: Definition -> AbsM A.Definition
tdefToAbs (TypeDefn tok name binders _ cons) = do
  cons' <- toAbstract cons
  return
    ( A.TypeDefn
        name
        binders
        cons'
        (maybeRangeOf tok <---> maybeRangeOf cons)
    )
tdefToAbs _ = error "defToAbs: Patterns exhausted. Shouldn't happen"

vdefsToAbs :: [Definition] -> AbsM [A.Definition]
vdefsToAbs = mapM desugarDefn . splitValDefns

splitValDefns :: [Definition] -> [(Name, Maybe Type, [Definition])]
splitValDefns [] = []
splitValDefns (ValDefnSig (DeclBase ns _ ty) : ds) =
  let (ds1, ds2) = span elemDefn ds
   in map (fetchDefn ds1) names ++ splitValDefns ds2
  where
    names = sepByToList ns
    elemDefn (ValDefn name' _ _ _) = name' `elem` names
    elemDefn _ = False
    sameDefn n (ValDefn name' _ _ _) = n == name'
    sameDefn _ _ = False
    fetchDefn dfns n = mkDefn . filter (sameDefn n) $ dfns
      where
        mkDefn ds' = (n, Just ty, ds')
splitValDefns (d@(ValDefn name _ _ _) : ds) =
  let (ds1, ds2) = span sameDefn ds
   in (name, Nothing, (d : ds1)) : splitValDefns ds2
  where
    sameDefn (ValDefn name' _ _ _) = name == name'
    sameDefn _ = False
splitValDefns _ = error "splitValDefns: Patterns exhausted. Shouldn't happen"

sepByToList :: SepBy sep a -> [a]
sepByToList (Head a) = [a]
sepByToList (Delim a _ as) = a : sepByToList as

desugarDefn :: (Name, Maybe Type, [Definition]) -> AbsM A.Definition
desugarDefn (name, ty, defns) = do
  A.ValDefn name
    <$> toAbstract ty
    <*> (desugarClauses (maybeRangeOf defns) =<< mapM extractFnClause defns)

extractFnClause :: Definition -> AbsM ([A.Pattern], A.Expr)
extractFnClause (ValDefn _ ptns _ body) =
  (,) <$> mapM toAbstract ptns <*> toAbstract body
extractFnClause _ = error "extractFnClause: Patterns exhausted. Shouldn't happen"

desugarClauses :: Maybe Range -> [([A.Pattern], A.Expr)] -> AbsM A.Expr
desugarClauses range clauses = do
  fnames <- freshNames (replicate arity (Text.pack "arg"))
  return $ wrapLam fnames (mkCase fnames)
  where
    arity = maximum (map (length . fst) clauses)
    mkCase :: [Name] -> A.Expr
    mkCase fnames =
      A.Case (A.Tuple (map nameToVar fnames)) (map mkCaseClause clauses) range
    mkCaseClause (ptns, body) = A.CaseClause (A.PattTuple ptns) body

instance ToAbstract TypeDefnCtor A.TypeDefnCtor where
  toAbstract (TypeDefnCtor c tys) = A.TypeDefnCtor c <$> mapM toAbstract tys

--------------------------------------------------------------------------------

-- | Declaraion
instance ToAbstract Declaration A.Declaration where
  toAbstract d = case d of
    ConstDecl _ decl -> do
      (name, body, prop) <- toAbstract decl
      return $ A.ConstDecl name body prop (maybeRangeOf d)
    VarDecl _ decl -> do
      (name, body, prop) <- toAbstract decl
      return $ A.VarDecl name body prop (maybeRangeOf d)

withRange :: (Monad m, MaybeRanged a) => (a -> m (Maybe Range -> b)) -> a -> m b
withRange f x = ($ maybeRangeOf x) <$> f x

--------------------------------------------------------------------------------

-- | Statement
instance ToAbstract Stmt A.Stmt where
  toAbstract stmt = withRange toAbstract' stmt
    where
      toAbstract' (Skip _) = return A.Skip
      toAbstract' (Abort _) = return A.Abort
      toAbstract' (Assign a _ b) = A.Assign <$> toAbstract a <*> toAbstract b
      toAbstract' (AAssign x _ i _ _ e) =
        A.AAssign (A.Var x (maybeRangeOf x)) <$> toAbstract i <*> toAbstract e
      toAbstract' (Assert _ a _) = A.Assert <$> toAbstract a
      toAbstract' (LoopInvariant _ a _ _ _ b _) =
        A.LoopInvariant <$> toAbstract a <*> toAbstract b
      toAbstract' (Do _ a _) = A.Do <$> toAbstract a
      toAbstract' (If _ a _) = A.If <$> toAbstract a
      toAbstract' (SpecQM _) = error "SpecQM should be digged before calling toAbstract"
      toAbstract' (Spec l xs r) = do
        let text = docToText $ toDoc $ prettyWithRange (map (fmap show) xs)
        return $ const (A.Spec text (rangeOf l <> rangeOf r))
      toAbstract' (Proof anchor contents _ r) = return $ const (A.Proof anchor contents r)
      toAbstract' (Alloc p _ _ _ es _) = A.Alloc p <$> toAbstract es
      toAbstract' (HLookup x _ _ e) = A.HLookup x <$> toAbstract e
      toAbstract' (HMutate _ e1 _ e2) = A.HMutate <$> toAbstract e1 <*> toAbstract e2
      toAbstract' (Dispose _ e) = A.Dispose <$> toAbstract e
      toAbstract' (Block _ p _) = A.Block <$> toAbstract p

instance ToAbstract GdCmd A.GdCmd where
  toAbstract (GdCmd a _ b) =
    A.GdCmd <$> toAbstract a <*> toAbstract b <*> pure (maybeRangeOf a <---> maybeRangeOf b)

--------------------------------------------------------------------------------

-- Low level Declaration wrapper, and synonym types
instance ToAbstract DeclBase ([Name], A.Type) where
  toAbstract (DeclBase a _ b) = (,) <$> toAbstract a <*> toAbstract b

instance ToAbstract DeclProp A.Expr where
  toAbstract (DeclProp _ e _) = toAbstract e

instance ToAbstract DeclType ([Name], A.Type, Maybe A.Expr) where
  toAbstract (DeclType decl prop) = do
    (ns, t) <- toAbstract decl
    e <- toAbstract prop
    return (ns, t, e)

--------------------------------------------------------------------------------

-- | Endpoint
instance ToAbstract EndpointOpen A.Endpoint where
  toAbstract (IncludingOpening _ a) = A.Including <$> toAbstract a
  toAbstract (ExcludingOpening _ a) = A.Excluding <$> toAbstract a

instance ToAbstract EndpointClose A.Endpoint where
  toAbstract (IncludingClosing a _) = A.Including <$> toAbstract a
  toAbstract (ExcludingClosing a _) = A.Excluding <$> toAbstract a

-- | Interval
instance ToAbstract Interval A.Interval where
  toAbstract i@(Interval a _ b) =
    A.Interval <$> toAbstract a <*> toAbstract b <*> pure (maybeRangeOf i)

-- | Base Type
instance ToAbstract TBase A.TBase where
  toAbstract (TInt _) = return A.TInt
  toAbstract (TBool _) = return A.TBool
  toAbstract (TChar _) = return A.TChar

-- | Type
-- Base types were recognized as TCon (because Base types and TCon are identical at the syntactical level),
-- and to be converted to TBase here.
instance ToAbstract Type A.Type where
  toAbstract t = case t of
    (TBase a) -> A.TBase <$> toAbstract a <*> pure (maybeRangeOf t)
    (TArray _ a _ b) ->
      A.TArray
        <$> toAbstract a
        <*> toAbstract b
        <*> pure (maybeRangeOf t)
    (TOp op) -> return $ A.TOp op
    (TData n _) -> return $ A.TData n (maybeRangeOf t)
    (TApp l r) ->
      A.TApp
        <$> toAbstract l
        <*> toAbstract r
        <*> pure (maybeRangeOf l <---> maybeRangeOf r)
    (TMetaVar a _) -> return $ A.TMetaVar a (maybeRangeOf t)
    (TParen _ a _) -> do
      t' <- toAbstract a
      case t' of
        A.TBase a' _ -> return $ A.TBase a' (maybeRangeOf t)
        A.TArray a' b' _ -> return $ A.TArray a' b' (maybeRangeOf t)
        A.TTuple as' -> return $ A.TTuple as'
        A.TFunc a' b' _ -> return $ A.TFunc a' b' (maybeRangeOf t)
        A.TOp op -> return $ A.TOp op
        A.TData name _ -> return $ A.TData name (maybeRangeOf t)
        A.TApp a' b' _ -> return $ A.TApp a' b' (maybeRangeOf t)
        A.TVar a' _ -> return $ A.TVar a' (maybeRangeOf t)
        A.TMetaVar a' _ -> return $ A.TMetaVar a' (maybeRangeOf t)
        A.TType -> return A.TType

--------------------------------------------------------------------------------

-- | Expressions
instance ToAbstract Expr A.Expr where
  toAbstract x = case x of
    Paren _ a _ -> toAbstract a
    Lit a -> A.Lit <$> toAbstract a <*> pure (maybeRangeOf x)
    Var a -> return $ A.Var a (maybeRangeOf x)
    Const a -> return $ A.Const a (maybeRangeOf x)
    Op a -> return $ A.Op a
    Chain ch -> A.Chain <$> toAbstract ch
    Arr arr _ i _ ->
      A.ArrIdx <$> toAbstract arr <*> toAbstract i <*> pure (maybeRangeOf x)
    App a b -> A.App <$> toAbstract a <*> toAbstract b <*> pure (maybeRangeOf x)
    Quant _ a b _ c _ d _ ->
      A.Quant (toAbstractQOp a) b
        <$> toAbstract c
        <*> toAbstract d
        <*> pure (maybeRangeOf x)
      where
        toAbstractQOp :: Either ArithOp Name -> A.Expr
        toAbstractQOp qop = case qop of
          Left op -> A.Op op
          Right n@(Name _ l) -> A.Const n l
    Case _ expr _ cases ->
      A.Case
        <$> toAbstract expr
        <*> toAbstract cases
        <*> pure (maybeRangeOf x)
    HoleQM _ -> error "SpecQM should be digged before calling toAbstract"
    Syntax.Concrete.Types.Hole l xs r -> do
      holeNumber <- countUp
      let text = docToText $ toDoc $ prettyWithRange (map (fmap show) xs)
       in return $ A.EHole text holeNumber (rangeOf l <> rangeOf r)

instance ToAbstract Chain A.Chain where
  toAbstract chain = case chain of
    Pure expr -> A.Pure <$> toAbstract expr <*> pure (maybeRangeOf expr)
    More ch' op expr ->
      A.More
        <$> toAbstract ch'
        <*> pure op
        <*> toAbstract expr
        <*> pure (maybeRangeOf expr)

instance ToAbstract CaseClause A.CaseClause where
  toAbstract (CaseClause patt _ body) =
    A.CaseClause <$> toAbstract patt <*> toAbstract body

instance ToAbstract Pattern A.Pattern where
  toAbstract (PattLit x) = A.PattLit <$> toAbstract x
  toAbstract (PattParen _ x _) = toAbstract x
  toAbstract (PattBinder x) = return $ A.PattBinder x
  toAbstract (PattWildcard x) = return $ A.PattWildcard (rangeOf x)
  toAbstract (PattConstructor ctor pats) =
    A.PattConstructor ctor <$> mapM toAbstract pats

-- | Literals (Integer / Boolean / Character)
instance ToAbstract Lit A.Lit where
  toAbstract (LitInt a _) = return $ A.Num a
  toAbstract (LitBool a _) = return $ A.Bol a
  toAbstract (LitChar a _) = return $ A.Chr a

--------------------------------------------------------------------------------

instance (ToAbstract a b) => ToAbstract (SepBy sep a) [b] where
  toAbstract (Head a) = (: []) <$> toAbstract a
  toAbstract (Delim a _ as) = (:) <$> toAbstract a <*> toAbstract as
