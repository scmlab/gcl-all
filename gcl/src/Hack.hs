{-# LANGUAGE PolyKinds #-}

module Hack where

import Control.Applicative ((<|>))
import Data.Loc (Loc (NoLoc), Located (..))
import Data.Loc.Range (Range (..))
import Data.Maybe (fromJust)
import Data.Monoid (getLast)
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Unsafe.Coerce (unsafeCoerce)

-- FIXME: this function is to help migrating LSP library versions
-- previous LSP uses `Int` for line and character offsets
-- current version uses `UInt`, causing type mismatches
-- this function also tries to help catch logical bugs if the
-- calculated value somehow turns out to be negative
-- this should eventually be removed in the future
intToUInt :: Int -> LSP.UInt
intToUInt a
  | a < 0 = error "negative integer detected where it shouldn't"
  | otherwise = toEnum a

uIntToInt :: LSP.UInt -> Int
uIntToInt = fromEnum

-- HACK: this is probably incorrect
-- see also:
-- - https://hackage.haskell.org/package/lsp-types-2.3.0.1/docs/Language-LSP-Protocol-Message.html#t:ResponseError
-- - https://hackage.haskell.org/package/lsp-types-2.3.0.1/docs/Language-LSP-Protocol-Message.html#t:TResponseError
-- - https://hackage.haskell.org/package/lsp-types-2.3.0.1/docs/Language-LSP-Protocol-Message.html#t:ErrorData
resToTRes :: LSP.ResponseError -> LSP.TResponseError m
resToTRes (LSP.ResponseError c m _) = LSP.TResponseError c m Nothing

-- WARN: not properly tested
-- hacky substitution for `Located.hs` definitions
info :: (Foldable f) => f a -> a
info = fromJust . maybeInfo

maybeInfo :: (Foldable f) => f a -> Maybe a
maybeInfo = getLast . foldMap pure

instance Located () where
  locOf _ = NoLoc

class IsRange a where
  (<-->) :: a -> a -> a

instance IsRange Range where
  -- WARN: not commutative
  (Range s1 _) <--> (Range _ e2) = Range s1 e2

instance (IsRange a) => IsRange (Maybe a) where
  (Just r1) <--> (Just r2) = Just (r1 <--> r2)
  r1 <--> r2 = r1 <|> r2

aToMaybeA :: (Functor f) => f a -> f (Maybe a)
aToMaybeA = fmap Just

-- WARN: unsafe function
aToRange :: a -> Range
aToRange = unsafeCoerce

-- WARN: unsafe function
rangeToA :: Range -> a
rangeToA = unsafeCoerce
