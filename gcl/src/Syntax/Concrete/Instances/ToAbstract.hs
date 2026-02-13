{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Syntax.Concrete.Instances.ToAbstract where

import Control.Monad.State.Lazy (State, evalState, get, put)
import GCL.Common (Counterous, countUp)
import GCL.Range (MaybeRanged (maybeRangeOf), rangeOf, (<--->))
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

instance Counterous AbstractTransformerM where
  countUp = do
    holeCount <- get
    put (succ holeCount)
    return holeCount

runAbstractTransform :: (ToAbstract a b) => a -> b
runAbstractTransform a = evalState (toAbstract a) 0

-- | Typeclass for converting Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> AbstractTransformerM b

instance (ToAbstract a b) => ToAbstract (Maybe a) (Maybe b) where
  toAbstract = mapM toAbstract

instance (ToAbstract a b) => ToAbstract [a] [b] where
  toAbstract = mapM toAbstract

instance ToAbstract Name Name where
  toAbstract = return

--------------------------------------------------------------------------------

-- | Program
instance ToAbstract Program A.Program where
  toAbstract prog@(Program ds stmts') = do
    (decls, defns) <- mconcat <$> toAbstract ds
    stmts <- toAbstract stmts'
    let (globProps, assertions) = ConstExpr.pickGlobals decls
        pre = [A.Assert (A.conjunct assertions) Nothing | not (null assertions)]
     in return $ A.Program defns decls globProps (pre ++ stmts) (maybeRangeOf prog)

instance ToAbstract (Either Declaration DefinitionBlock) ([A.Declaration], [A.Definition]) where
  toAbstract (Left d) = do
    decls <- toAbstract d
    return ([decls], [])
  toAbstract (Right defnBlock) = do
    defns <- toAbstract defnBlock
    return ([], defns)

--------------------------------------------------------------------------------

-- | Definition
instance ToAbstract DefinitionBlock [A.Definition] where
  toAbstract (DefinitionBlock _ defns _) = concat <$> toAbstract defns

instance ToAbstract Definition [A.Definition] where
  toAbstract (TypeDefn tok name binders _ cons') = do
    cons <- toAbstract cons'
    return [A.TypeDefn name binders cons (maybeRangeOf tok <---> maybeRangeOf cons)]
  toAbstract (FuncDefnSig decl prop') = do
    prop <- toAbstract prop'
    (names, typ) <- toAbstract decl
    return $ map (\n -> A.FuncDefnSig n typ prop (maybeRangeOf decl <---> (maybeRangeOf =<< prop))) names
  toAbstract (FuncDefn name args _ body') = do
    body <- toAbstract body'
    return [A.FuncDefn name (wrapLam args body)]

instance ToAbstract TypeDefnCtor A.TypeDefnCtor where
  toAbstract (TypeDefnCtor c typs') = do
    A.TypeDefnCtor c <$> mapM toAbstract typs'

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

--------------------------------------------------------------------------------

-- | Statement
instance ToAbstract Stmt A.Stmt where
  toAbstract stmt = case stmt of
    Skip _ -> return $ A.Skip (maybeRangeOf stmt)
    Abort _ -> return $ A.Abort (maybeRangeOf stmt)
    Assign a' _ b' -> do
      a <- toAbstract a'
      b <- toAbstract b'
      return $ A.Assign a b (maybeRangeOf stmt)
    AAssign x _ i' _ _ e' -> do
      i <- toAbstract i'
      e <- toAbstract e'
      return $ A.AAssign (A.Var x (maybeRangeOf x)) i e (maybeRangeOf stmt)
    Assert _ a' _ -> do
      a <- toAbstract a'
      return $ A.Assert a (maybeRangeOf stmt)
    LoopInvariant _ a' _ _ _ b' _ -> do
      a <- toAbstract a'
      b <- toAbstract b'
      return $ A.LoopInvariant a b (maybeRangeOf stmt)
    Do _ a' _ -> do
      a <- toAbstract a'
      return $ A.Do a (maybeRangeOf stmt)
    If _ a' _ -> do
      a <- toAbstract a'
      return $ A.If a (maybeRangeOf stmt)
    SpecQM _ -> error "SpecQM should be digged before calling toAbstract"
    Spec l xs r ->
      let text = docToText $ toDoc $ prettyWithRange (map (fmap show) xs)
       in return $ A.Spec text (rangeOf l <> rangeOf r)
    Proof anchor contents _ r -> return $ A.Proof anchor contents r
    Alloc p _ _ _ es' _ -> do
      es <- toAbstract es'
      return $ A.Alloc p es (maybeRangeOf stmt)
    HLookup x _ _ e' -> do
      e <- toAbstract e'
      return $ A.HLookup x e (maybeRangeOf stmt)
    HMutate _ e1' _ e2' -> do
      e1 <- toAbstract e1'
      e2 <- toAbstract e2'
      return $ A.HMutate e1 e2 (maybeRangeOf stmt)
    Dispose _ e' -> do
      e <- toAbstract e'
      return $ A.Dispose e (maybeRangeOf stmt)
    Block _ p' _ -> do
      p <- toAbstract p'
      return $ A.Block p (maybeRangeOf stmt)

instance ToAbstract GdCmd A.GdCmd where
  toAbstract (GdCmd a' _ b') = do
    a <- toAbstract a'
    b <- toAbstract b'
    return $ A.GdCmd a b (maybeRangeOf a <---> maybeRangeOf b)

--------------------------------------------------------------------------------

-- Low level Declaration wrapper, and synonym types
instance ToAbstract DeclBase ([Name], A.Type) where
  toAbstract (DeclBase a _ b) = liftA2 (,) (toAbstract a) (toAbstract b)

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
  toAbstract i@(Interval a' _ b') = do
    a <- toAbstract a'
    b <- toAbstract b'
    return $ A.Interval a b (maybeRangeOf i)

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
    (TBase a') -> do
      a <- toAbstract a'
      return $ A.TBase a (maybeRangeOf t)
    (TArray _ a' _ b') -> do
      a <- toAbstract a'
      b <- toAbstract b'
      return $ A.TArray a b (maybeRangeOf t)
    (TOp op) -> return $ A.TOp op
    (TData n _) -> return $ A.TData n (maybeRangeOf t)
    (TApp l' r') -> do
      l <- toAbstract l'
      r <- toAbstract r'
      return $ A.TApp l r (maybeRangeOf l <---> maybeRangeOf r)
    (TMetaVar a _) -> return $ A.TMetaVar a (maybeRangeOf t)
    (TParen _ a _) -> do
      t' <- toAbstract a
      return $ case t' of
        A.TBase a' _ -> A.TBase a' (maybeRangeOf t)
        A.TArray a' b' _ -> A.TArray a' b' (maybeRangeOf t)
        A.TTuple as' -> A.TTuple as'
        A.TFunc a' b' _ -> A.TFunc a' b' (maybeRangeOf t)
        A.TOp op -> A.TOp op
        A.TData name _ -> A.TData name (maybeRangeOf t)
        A.TApp a' b' _ -> A.TApp a' b' (maybeRangeOf t)
        A.TVar a' _ -> A.TVar a' (maybeRangeOf t)
        A.TMetaVar a' _ -> A.TMetaVar a' (maybeRangeOf t)
        A.TType -> A.TType

--------------------------------------------------------------------------------

-- | Expressions
instance ToAbstract Expr A.Expr where
  toAbstract x = case x of
    Paren _ a _ -> toAbstract a
    Lit a' -> do
      a <- toAbstract a'
      return $ A.Lit a (maybeRangeOf x)
    Var a -> return $ A.Var a (maybeRangeOf x)
    Const a -> return $ A.Const a (maybeRangeOf x)
    Op a -> return $ A.Op a
    Chain ch' -> A.Chain <$> toAbstract ch'
    Arr arr' _ i' _ -> do
      arr <- toAbstract arr'
      i <- toAbstract i'
      return $ A.ArrIdx arr i (maybeRangeOf x)
    App a' b' -> do
      a <- toAbstract a'
      b <- toAbstract b'
      return $ A.App a b (maybeRangeOf x)
    Quant _ a b _ c' _ d' _ -> do
      c <- toAbstract c'
      d <- toAbstract d'
      return $ A.Quant (toAbstractQOp a) b c d (maybeRangeOf x)
      where
        toAbstractQOp :: Either ArithOp Name -> A.Expr
        toAbstractQOp qop = case qop of
          Left op -> A.Op op
          Right n@(Name _ l) -> A.Const n l
    Case _ expr' _ cases' -> do
      expr <- toAbstract expr'
      cases <- toAbstract cases'
      return $ A.Case expr cases (maybeRangeOf x)
    HoleQM _ -> error "SpecQM should be digged before calling toAbstract"
    Hole l xs r -> do
      holeNumber <- countUp
      let text = docToText $ toDoc $ prettyWithRange (map (fmap show) xs)
       in return $ A.EHole text holeNumber (rangeOf l <> rangeOf r)

instance ToAbstract Chain A.Chain where
  toAbstract chain = case chain of
    Pure expr' -> do
      expr <- toAbstract expr'
      return $ A.Pure expr (maybeRangeOf expr)
    More ch' op expr' -> do
      ch <- toAbstract ch'
      expr <- toAbstract expr'
      return $ A.More ch op expr (maybeRangeOf expr)

instance ToAbstract CaseClause A.CaseClause where
  toAbstract (CaseClause patt' _ body') = do
    patt <- toAbstract patt'
    body <- toAbstract body'
    return $ A.CaseClause patt body

instance ToAbstract Pattern A.Pattern where
  toAbstract (PattLit x) = A.PattLit <$> toAbstract x
  toAbstract (PattParen _ x _) = toAbstract x
  toAbstract (PattBinder x) = return $ A.PattBinder x
  toAbstract (PattWildcard x) = return $ A.PattWildcard (rangeOf x)
  toAbstract (PattConstructor ctor pats') = A.PattConstructor ctor <$> toAbstract pats'

-- | Literals (Integer / Boolean / Character)
instance ToAbstract Lit A.Lit where
  toAbstract (LitInt a _) = return $ A.Num a
  toAbstract (LitBool a _) = return $ A.Bol a
  toAbstract (LitChar a _) = return $ A.Chr a

--------------------------------------------------------------------------------

instance (ToAbstract a b) => ToAbstract (SepBy sep a) [b] where
  toAbstract (Head a) = pure <$> toAbstract a
  toAbstract (Delim a _ as) = liftA2 (:) (toAbstract a) (toAbstract as)
