{-# LANGUAGE DeriveGeneric #-}

module GCL.Type where

import GCL.Range (MaybeRanged (maybeRangeOf), Range)
import GHC.Generics
import Syntax.Abstract
import Syntax.Common

data TypeError
  = NotInScope Name
  | UnifyFailed Type Type (Maybe Range)
  | KindUnifyFailed Kind Kind (Maybe Range)
  | RecursiveType Name Type (Maybe Range)
  | AssignToConst Name
  | UndefinedType Name
  | DuplicatedIdentifiers [Name]
  | RedundantNames [Name]
  | RedundantExprs [Expr]
  | MissingArguments [Name]
  | PatternArityMismatch {- Expected -} Int {- Actual -} Int (Maybe Range)
  deriving (Show, Eq, Generic)

instance MaybeRanged TypeError where
  maybeRangeOf (NotInScope n) = maybeRangeOf n
  maybeRangeOf (UnifyFailed _ _ l) = l
  maybeRangeOf (KindUnifyFailed _ _ l) = l
  maybeRangeOf (RecursiveType _ _ l) = l
  maybeRangeOf (AssignToConst n) = maybeRangeOf n
  maybeRangeOf (UndefinedType n) = maybeRangeOf n
  maybeRangeOf (DuplicatedIdentifiers ns) = maybeRangeOf ns
  maybeRangeOf (RedundantNames ns) = maybeRangeOf ns
  maybeRangeOf (RedundantExprs exprs) = maybeRangeOf exprs
  maybeRangeOf (MissingArguments ns) = maybeRangeOf ns
  maybeRangeOf (PatternArityMismatch _ _ l) = l
