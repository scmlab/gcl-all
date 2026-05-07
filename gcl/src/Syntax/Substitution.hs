{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax.Substitution where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GCL.Common
import GCL.Range (Range)
import Syntax.Common

type Subst b = [(Text, b)] -- SCM: I think it's an overkill using Map Text b

class Substitutable m a b where
  subst :: Subst b -> a -> m a

-- types that has a concept of a "variable"
class Variableous e t | e -> t where -- t denotes the type of the variable
  isVar :: e -> Maybe (Name, t)
  mkVar :: Name -> t -> Maybe Range -> e

-- structual instance declarations

instance (Monad m, Substitutable m a b) => Substitutable m [a] b where
  subst sb = mapM (subst sb)

instance
  (Monad m, Substitutable m a c, Substitutable m b c) =>
  Substitutable m (a, b) c
  where
  subst sb (x, y) = (,) <$> subst sb x <*> subst sb y

instance
  ( Monad m,
    Substitutable m a d,
    Substitutable m b d,
    Substitutable m c d
  ) =>
  Substitutable m (a, b, c) d
  where
  subst sb (x, y, z) = do
    x' <- subst sb x
    y' <- subst sb y
    z' <- subst sb z
    return (x', y', z')

instance
  (Monad m, Substitutable m a b) =>
  Substitutable m (Maybe a) b
  where
  subst sb (Just x) = Just <$> subst sb x
  subst _ Nothing = return Nothing

-- A common pattern: performing substitution on an
-- expression with binders, e.g
--       (\ xs -> e) sb = (\ xs' -> e')
--   where (xs', e') <- substBinder sb xs e.
-- It performs substitution on e, while renaming xs if necessary.

substBinder ::
  ( Fresh m,
    Variableous e t,
    Substitutable m a e,
    Free e,
    Free a
  ) =>
  Subst e ->
  [(Name, t)] ->
  a ->
  m ([(Name, t)], a, Subst e)
substBinder sb binders body = do
  sb'' <- genBinderRenaming fvsb binders
  let binders' = zip (renameVars sb'' (map fst binders)) (map snd binders)
  let sbnew = sb'' <> sb'
  body' <- subst sbnew body
  return $ (binders', body', sbnew)
  where
    sb' = shrinkSubst (map fst binders) (freeVarsT body) sb
    fvsb = Set.unions . map freeVarsT . elems $ sb'

-- Utilities

shrinkSubst :: [Name] -> Set Text -> Subst b -> Subst b
shrinkSubst binders ns subs =
  restrictKeys (substractKeys subs (map nameToText binders)) ns
  where
    substractKeys sb bs =
      filterWithKey (\k _ -> not (k `elem` bs)) sb

genBinderRenaming ::
  (Fresh m, Variableous e t) =>
  Set Text ->
  [(Name, t)] ->
  m (Subst e)
genBinderRenaming _ [] = return empty
genBinderRenaming fvs ((Name x l, t) : xs)
  | x `Set.member` fvs = do
      x' <- freshName x l
      insert x (mkVar x' t l) <$> genBinderRenaming fvs xs
  | otherwise = genBinderRenaming fvs xs

renameVars :: (Variableous e t) => Subst e -> [Name] -> [Name]
renameVars sb = map (renameVar sb)

renameVar :: (Variableous e t) => Subst e -> Name -> Name
renameVar sb x = case lookup (nameToText x) sb of
  Nothing -> x
  Just e -> case isVar e of
    Just (y, _) -> y
    Nothing -> error "variable should be substituted for a variable"

--- SCM: we assume that renameVars always succeed.
--       Do we need to raise a catchable error?


-- list verson of Data.Map methods for Subst

elems :: Subst b -> [b]
elems = map snd

empty :: Subst b
empty = []

filterWithKey :: (Text -> b -> Bool) -> Subst b -> Subst b
filterWithKey p = filter (uncurry p)

restrictKeys :: Subst b -> Set Text -> Subst b
restrictKeys sb ns = filter ((`Set.member` ns) . fst) sb

insert :: Text -> b -> Subst b -> Subst b
insert n e = ((n, e):)

keys :: Subst b -> [Text]
keys = map fst
