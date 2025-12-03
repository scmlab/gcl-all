{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module GCL.Exec.ExecMonad where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson
import Data.Loc.Range (Range, MaybeRanged(..))
import GHC.Generics
import Syntax.Abstract.Types (GdCmd, Var)

type Store = [(Var, Val)]

data Val
  = VNum Int
  | VBol Bool
  | VChr Char
  | VFun (Val -> Either ExecError Val)
  | VArr Int [Val]
  | Undef

data ExecError
  = Aborted (Maybe Range)
  | AllFailedInIf (Maybe Range)
  | DivByZero (Maybe Range)
  | ArrayOutOfBound Int Int (Maybe Range)
  deriving (Show, Eq, Generic)

instance ToJSON ExecError

instance MaybeRanged ExecError where
  maybeRangeOf (Aborted l) = l
  maybeRangeOf (AllFailedInIf l) = l
  maybeRangeOf (DivByZero l) = l
  maybeRangeOf (ArrayOutOfBound _ _ l) = l

class
  (MonadPlus m, MonadError ExecError m, MonadState Store m) =>
  ExecMonad m
  where
  lookupStore :: Maybe Range -> Var -> m Val
  updateStore :: Maybe Range -> Var -> Val -> m ()
  shuffle :: [GdCmd] -> m [GdCmd]

  lookupStore l x =
    (lookup x <$> get) >>= \case
      Nothing -> error "shouldn't happen"
      Just (VArr n xs) -> return (VFun (arrToFun l n xs))
      Just v -> return v

  updateStore _ x v = do
    store <- get
    put ((x, v) : filter (not . (== x) . fst) store)

  shuffle = return

arrToFun :: Maybe Range -> Int -> [a] -> Val -> Either ExecError a
arrToFun l n xs (VNum i)
  | i < n = Right (xs !! i)
  | otherwise = Left (ArrayOutOfBound i n l)
arrToFun _ _ _ _ = error "type error, shouldn't hapen"

instance Show Val where
  showsPrec p (VNum i) = showsPrec p i
  showsPrec p (VBol b) = showsPrec p b
  showsPrec p (VChr c) = showsPrec p c
  showsPrec _ (VFun _) = ("<Fun>" ++)
  showsPrec p (VArr _ xs) = showsPrec p xs
  showsPrec _ Undef = ("undef" ++)
