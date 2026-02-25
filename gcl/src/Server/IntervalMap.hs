{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.IntervalMap
  ( IntervalMap,
    singleton,
    toList,
    fromList,
    fromAscList,
    insert,
    lookup,
    split,
    Scope,
    M,
    runM,
    Collect (..),
    lookupScopes,
    localScope,
  )
where

import Control.Monad.RWS
import Data.Bifunctor (bimap)
import Data.Foldable (forM_)
import qualified Data.Foldable as Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GCL.Range
  ( Pos,
    Range,
    posOrd,
    rangeEnd,
    rangeStart,
  )
import Prettyprinter
import Syntax.Concrete (SepBy)
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- Uses IntMap internally for speeding up lookups
-- with the key of IntMap acting as the starting posOrd (ordering key),
-- and the element's Int acting as the ending posOrd
newtype IntervalMap value = IntervalMap (IntMap (Int, value)) deriving (Eq, Monoid, Semigroup)

instance Functor IntervalMap where
  fmap f (IntervalMap m) = IntervalMap (IntMap.map (fmap f) m)

-- Instances for debugging
instance (Pretty value) => Show (IntervalMap value) where
  show = show . pretty

instance (Pretty value) => Pretty (IntervalMap value) where
  pretty (IntervalMap xs) =
    vcat
      $ Prelude.map
        ( \(start, (end, value)) ->
            "(" <> pretty start <> ", " <> pretty end <> ") => " <> pretty value
        )
      $ IntMap.toList xs

instance Foldable IntervalMap where
  foldMap f (IntervalMap xs) = foldMap (f . snd) xs

--------------------------------------------------------------------------------
-- Construction

-- Constructs a IntervalMap with a Range and a payload
singleton :: Range -> value -> IntervalMap value
singleton range value =
  IntervalMap $
    IntMap.singleton
      (posOrd (rangeStart range))
      (posOrd (rangeEnd range), value)

toList :: IntervalMap value -> [((Int, Int), value)]
toList (IntervalMap m) = map (\(a, (b, c)) -> ((a, b), c)) (IntMap.toList m)

fromList :: [((Int, Int), value)] -> IntervalMap value
fromList = IntervalMap . IntMap.fromList . map (\((a, b), c) -> (a, (b, c)))

fromAscList :: [((Int, Int), value)] -> IntervalMap value
fromAscList = IntervalMap . IntMap.fromAscList . map (\((a, b), c) -> (a, (b, c)))

--------------------------------------------------------------------------------
-- Insertion

insert :: Range -> value -> IntervalMap value -> IntervalMap value
insert range value (IntervalMap m) =
  IntervalMap $
    IntMap.insert
      (posOrd (rangeStart range))
      (posOrd (rangeEnd range), value)
      m

split :: Range -> IntervalMap value -> (IntervalMap value, IntervalMap value)
split rng (IntervalMap m) =
  bimap IntervalMap IntervalMap (IntMap.split (posOrd (rangeStart rng)) m)

--------------------------------------------------------------------------------
-- Query

-- Given a Pos, returns the paylod and its Range if the Pos is within its Range
lookup' :: Pos -> IntervalMap value -> Maybe ((Int, Int), value)
lookup' pos (IntervalMap m) =
  let ord = posOrd pos
   in case IntMap.lookupLE ord m of
        Nothing -> Nothing
        Just (start, (end, x)) ->
          if ord < end then Just ((start, end), x) else Nothing

-- Given a Pos, returns the paylod if the Pos is within its Range
lookup :: Pos -> IntervalMap value -> Maybe value
lookup pos m = snd <$> lookup' pos m

--------------------------------------------------------------------------------

-- | A mapping from names to something else
--   for go to definition: Scope LocationLinkToBe
--   for syntax highlighting: Scope ()
type Scope input = Map Text input

-- | Accumulates the result of `IntervalMap` in writer
--   Stores stack of scopes in reader
type M input output = RWS [Scope input] (IntervalMap output) ()

runM :: [Scope input] -> M input output a -> IntervalMap output
runM scopes f = let (_, _, w) = runRWS f scopes () in w

-- | See if a name is in a series of scopes (from local to global)
-- | Return the first result (which should be the most local target)
lookupScopes :: Text -> M input output (Maybe input)
lookupScopes name = asks findFirstInScopes
  where
    findFirstInScopes :: [Scope input] -> Maybe input
    findFirstInScopes [] = Nothing
    findFirstInScopes (s : ss) = case Map.lookup name s of
      Just v -> Just v
      Nothing -> findFirstInScopes ss

localScope :: (MonadReader [Scope input] m) => Scope input -> m a -> m a
localScope scope = local (scope :)

--------------------------------------------------------------------------------

-- | Given a Abstract syntax node, returns a mapping of Range-Info
class Collect input output a where
  collect :: a -> M input output ()

instance (Collect input output a) => Collect input output (Maybe a) where
  collect Nothing = return ()
  collect (Just x) = collect x

instance (Collect input output a) => Collect input output [a] where
  collect = mapM_ collect

instance (Collect input output a) => Collect input output (NonEmpty a) where
  collect = mapM_ collect

instance (Collect input output a) => Collect input output (Map k a) where
  collect = mapM_ collect

instance (Collect input output a, Collect input output b) => Collect input output (Either a b) where
  collect (Left a) = collect a
  collect (Right a) = collect a

instance (Collect input output a) => Collect input output (SepBy tok a) where
  collect xs = forM_ (Foldable.toList xs) collect
