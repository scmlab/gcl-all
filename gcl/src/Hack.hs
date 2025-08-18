module Hack where

import Language.LSP.Protocol.Types (UInt)

-- FIXME: this function is to help migrating LSP library versions
-- previous LSP uses `Int` for line and character offsets
-- current version uses `UInt`, causes type mismatches
-- this function also tries to help catch logical bugs if the
-- calculated value somehow turns out to be negative
-- this should eventually be removed in the future
intToUInt :: Int -> UInt
intToUInt a
  | a < 0 = error "negative integer detected where it shouldn't"
  | otherwise = toEnum a

uIntToInt :: UInt -> Int
uIntToInt = fromEnum
