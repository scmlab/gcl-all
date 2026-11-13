{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Syntax.Concrete.Instances.ToAbstractLN where

import Control.Monad.Except (Except, throwError)
import Control.Monad.Reader (ReaderT (..), local, reader)
import Control.Monad.State
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Text (Text)
import GCL.Common
import GCL.Range (Range, MaybeRanged (..), maybeRangeOf, rangeOf, (<--->))
import Pretty.Util
  ( PrettyWithRange (prettyWithRange),
    docToText,
    toDoc,
  )
import qualified Syntax.AbstractLN.Types as A
import Syntax.Common (ArithOp, Name (..), nameToText)
import Syntax.Concrete.Types
import Syntax.Concrete.Instances.Located ()

type AbsM a = ReaderT [(Text, Int)] (StateT HoleCount (Except ScopeError)) a

type HoleCount = Int

data ScopeError =
        VarNotInScope Text (Maybe Range)
      | DuplicatedIdentifiers Text [Maybe Range]
      | ParAssignmentMismatch (Maybe Range)
    deriving Show


-- | Typeclass for converting Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> AbsM b

instance (ToAbstract a b) => ToAbstract (Maybe a) (Maybe b) where
  toAbstract Nothing = return Nothing
  toAbstract (Just x) = Just <$> toAbstract x

instance (ToAbstract a b, ToAbstract c d) => ToAbstract (Either a c) (Either b d) where
  toAbstract = bitraverse toAbstract toAbstract

instance (ToAbstract a b) => ToAbstract [a] [b] where
  toAbstract = mapM toAbstract

--------------------------------------------------------------------------------

lookupVar :: Text -> AbsM (Maybe Int)
lookupVar x = reader (lookup x)

withLocalVars :: [Text] -> AbsM a -> AbsM a
withLocalVars xs = local ext
  where ext :: [(Text, Int)]  -> [(Text, Int)]
        ext env = zip (xs ++ map fst env) [0..]

--------------------------------------------------------------------------------
-- | Statement

withRange :: (Monad m, MaybeRanged a) => (a -> m (Maybe Range -> b)) -> a -> m b
withRange f x = ($ maybeRangeOf x) <$> f x

instance ToAbstract Stmt A.Stmt where
 toAbstract stmt = withRange toAbstract' stmt
  where
    toAbstract' (Skip _) = return A.Skip
    toAbstract' (Abort _) = return A.Abort
    toAbstract' a@(Assign xs _ es) =
      A.Assign <$> handleParAssign (sepToList xs) (sepToList es) (maybeRangeOf a)
    toAbstract' (AAssign x _ i _ _ e) =
      A.AAssign <$> (A.Var . flip A.BName l <$> lookupOrRaise (nameToText x) l)
        <*> toAbstract i <*> toAbstract e
      where l = maybeRangeOf x
    toAbstract' (Assert _ a _) = A.Assert <$> toAbstract a
    toAbstract' (LoopInvariant _ a _ _ _ b _) =
      A.LoopInvariant <$> toAbstract a <*> toAbstract b
    toAbstract' (Do _ a _) = A.Do <$> toAbstract a
    toAbstract' (If _ a _) = A.If <$> toAbstract a
    toAbstract' (SpecQM _) = error "SpecQM should be digged before calling toAbstract"
    toAbstract' (Spec l xs r) = do
      let text = docToText $ toDoc $ prettyWithRange (map (fmap show) xs)
      return $ const (A.Spec text (rangeOf l <> rangeOf r))
    toAbstract' (Alloc p _ _ _ es _) =
      A.Alloc <$> (flip A.BName l <$> lookupOrRaise (nameToText p) l)
              <*> toAbstract es
      where l = maybeRangeOf p
    toAbstract' (HLookup x _ _ e) =
      A.HLookup <$> (flip A.BName l <$> lookupOrRaise (nameToText x) l)
                <*> toAbstract e
      where l = maybeRangeOf x
    toAbstract' (HMutate _ e1 _ e2) = A.HMutate <$> toAbstract e1 <*> toAbstract e2
    toAbstract' (Dispose _ e) = A.Dispose <$> toAbstract e
    toAbstract' (Block _ p _) = undefined -- A.Block <$> toAbstract p

    handleParAssign :: [Either Name Hole] -> [Expr] -> Maybe Range
                    -> AbsM [(Either A.Var A.Hole, A.Expr)]
    handleParAssign xs es l
     | length xs /= length es = throwError (ParAssignmentMismatch l)
     | otherwise = mapM asgn (zip xs es)
    asgn (Left (Name x l), e) = do
       i <- lookupOrRaise x l
       e' <- toAbstract e
       return (Left (A.BName i l), e')
    asgn (Right h, e) =
       (,) <$> (Right <$> toAbstract h) <*> toAbstract e

instance ToAbstract GdCmd A.GdCmd where
  toAbstract (GdCmd a _ b) =
     A.GdCmd <$> toAbstract a <*> toAbstract b <*> pure (maybeRangeOf a <---> maybeRangeOf b)

--------------------------------------------------------------------------------
-- | Expressions

instance ToAbstract Expr A.Expr where
 toAbstract (Paren _ e _) = toAbstract e
 toAbstract (Lit l) = A.Lit <$> toAbstract l <*> pure (maybeRangeOf l)
 toAbstract (Var (Name x l)) =
   A.Var . flip A.BName l <$> lookupOrRaise x l
 toAbstract (Const (Name x l)) =
   A.Const . flip A.BName l <$> lookupOrRaise x l
 toAbstract (Op op) = return (A.Op op)
 toAbstract (Chain ch) = A.Chain <$> toAbstract ch
 toAbstract e@(Arr arr _ i l) =
   A.ArrIdx <$> toAbstract arr <*> toAbstract i <*> pure (maybeRangeOf e)
 toAbstract e@(App e0 e1) =
   A.App <$> toAbstract e0 <*> toAbstract e1 <*> pure (maybeRangeOf e)
 toAbstract e@(Quant _ op xs _ ran _ bod _) =
   A.Quant <$> toAbstractQOp op <*> pure xs'
    <*> withLocalVars xs' (toAbstract ran)
    <*> withLocalVars xs' (toAbstract bod)
    <*> pure (maybeRangeOf e)
   where
     xs' = map nameToText xs
     toAbstractQOp :: Either ArithOp Name -> AbsM A.Expr
     toAbstractQOp qop = case qop of
       Left op -> return (A.Op op)
       Right n@(Name _ l) -> toAbstract (Const n)
 toAbstract e@(Case _ m _ cls) =
    A.Case
      <$> toAbstract m
      <*> toAbstract cls
      <*> pure (maybeRangeOf e)
 toAbstract (EHole h) = A.EHole <$> toAbstract h

lookupOrRaise :: Text -> Maybe Range -> AbsM Int
lookupOrRaise x l =
  lookupVar x >>=
  maybe (throwError (VarNotInScope x l)) return

instance ToAbstract Lit A.Lit where
  toAbstract (LitInt a _) = return $ A.Num a
  toAbstract (LitBool a _) = return $ A.Bol a
  toAbstract (LitChar a _) = return $ A.Chr a

instance ToAbstract Chain A.Chain where
  toAbstract (Pure e) = A.Pure <$> toAbstract e <*> pure (maybeRangeOf e)
  toAbstract c@(More ch op e) =
     A.More <$> toAbstract ch
            <*> pure op
            <*> toAbstract e
            <*> pure (maybeRangeOf c)

instance ToAbstract CaseClause A.CaseClause where
  toAbstract (CaseClause patt _ body) =
     case findDuplicate xs of
       Nothing -> A.CaseClause <$> toAbstract patt <*>
                     withLocalVars xs' (toAbstract body)
       Just (x,ys) -> throwError (DuplicatedIdentifiers (nameToText x)
                                    (map maybeRangeOf (x:ys)))
    where xs = extractBinders patt
          xs' = map nameToText xs

instance ToAbstract Pattern A.Pattern where
  toAbstract (PattLit x) = A.PattLit <$> toAbstract x
  toAbstract (PattParen _ x _) = toAbstract x
  toAbstract (PattBinder x) = return $ A.PattBinder (nameToText x)
  toAbstract (PattWildcard x) = return $ A.PattWildcard (rangeOf x)
  toAbstract (PattConstructor ctor pats) =
    A.PattConstructor ctor <$> mapM toAbstract pats

instance ToAbstract Hole A.Hole where
  toAbstract (HoleQM _) = error "HoleQM should be digged before calling toAbstract"
  toAbstract (Syntax.Concrete.Types.Hole l xs r) = do
    holeNumber <- countUp
    let text = docToText $ toDoc $ prettyWithRange (map (fmap show) xs)
     in return $ A.Hole text holeNumber (rangeOf l <> rangeOf r)

--

extractBinders :: Pattern -> [Name]
extractBinders (PattLit _) = []
extractBinders (PattParen _ p _) = extractBinders p
extractBinders (PattBinder x) = [x]
extractBinders (PattWildcard _) = []
extractBinders (PattConstructor _ xs) = xs >>= extractBinders

  -- Given a list of Names, find the first set of duplicate names, if any
  -- This is for error reporting, and we need not find all duplicates.

findDuplicate :: [Name] -> Maybe (Name, [Name])
findDuplicate [] = Nothing
findDuplicate (x:xs) =
    case filter (x==) xs of
      [] -> findDuplicate xs
      ys -> Just (x,ys)


sepToList :: SepBy s a -> [a]
sepToList (Head x) = [x]
sepToList (Delim x _ xs) = x : sepToList xs

--------------------------------------------------------------------------------

instance (ToAbstract a b) => ToAbstract (SepBy sep a) [b] where
  toAbstract (Head a) = (: []) <$> toAbstract a
  toAbstract (Delim a _ as) = (:) <$> toAbstract a <*> toAbstract as
