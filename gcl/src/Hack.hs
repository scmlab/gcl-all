{-# LANGUAGE PolyKinds #-}

module Hack where

import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP

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
