{-# LANGUAGE PolyKinds #-}

module Hack where

import GCL.Type (TypeError (..))
import qualified GCL.Type2.Types as Type2
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

toOldError :: Type2.TypeError -> TypeError
toOldError (Type2.NotInScope n) = NotInScope n
toOldError (Type2.UnifyFailed t1 t2 r) = UnifyFailed t1 t2 r
toOldError (Type2.RecursiveType n t r) = RecursiveType n t r
toOldError (Type2.AssignToConst n) = AssignToConst n
toOldError (Type2.UndefinedType n) = UndefinedType n
toOldError (Type2.DuplicatedIdentifiers ns) = DuplicatedIdentifiers ns
toOldError (Type2.RedundantNames ns) = RedundantNames ns
toOldError (Type2.RedundantExprs es) = RedundantExprs es
toOldError (Type2.MissingArguments ns) = MissingArguments ns
toOldError (Type2.PatternArityMismatch expected actual r) = PatternArityMismatch expected actual r
