{-# LANGUAGE PolyKinds #-}

module Hack where

import Data.List (foldl')
import GCL.Range (MaybeRanged (..))
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Pretty.Util (PrettyWithRange (..))

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

sshow :: (Show a) => a -> String
sshow x = go 0 (show x)
  where
    indent n = replicate (2 * n) ' '

    go _ [] = ""
    go n (c : cs)
      | c == '[' = "[\n" ++ indent (n + 1) ++ go (n + 1) cs
      | c == ']' = "\n" ++ indent (n - 1) ++ "]" ++ go (n - 1) cs
      | c == ',' = ",\n" ++ indent n ++ go n cs
      | c == '(' = "\n" ++ indent n ++ "(" ++ go (n + 1) cs
      | c == ')' = "\n" ++ indent (n - 1) ++ ")" ++ go (n - 1) cs
      | c == ' ' = " " ++ go n cs
      | otherwise = c : go n cs

data Choice3 a b c
  = A a
  | B b
  | C c
  deriving (Eq, Show)

choice3 :: (a -> d) -> (b -> d) -> (c -> d) -> Choice3 a b c -> d
choice3 f _ _ (A a) = f a
choice3 _ g _ (B b) = g b
choice3 _ _ h (C c) = h c

instance (MaybeRanged a, MaybeRanged b, MaybeRanged c) => MaybeRanged (Choice3 a b c) where
  maybeRangeOf = choice3 maybeRangeOf maybeRangeOf maybeRangeOf

instance (PrettyWithRange a, PrettyWithRange b, PrettyWithRange c) => PrettyWithRange (Choice3 a b c) where
  prettyWithRange = choice3 prettyWithRange prettyWithRange prettyWithRange

partitionChoice3 :: [Choice3 a b c] -> ([a], [b], [c])
partitionChoice3 =
  foldl'
    ( \(as, bs, cs) choice ->
        case choice of
          A a -> (a : as, bs, cs)
          B b -> (as, b : bs, cs)
          C c -> (as, bs, c : cs)
    )
    ([], [], [])
