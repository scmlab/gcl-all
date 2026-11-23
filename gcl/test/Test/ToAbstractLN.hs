{-# LANGUAGE OverloadedStrings #-}

module Test.ToAbstractLN (tests) where

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Syntax.AbstractLN as A
import Syntax.Common (nameToText)
import Syntax.Concrete.Instances.ToAbstractLN (toAbstract)
import qualified Syntax.Parser as Parser
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "ToAbstractLN"
    [ -- {:
      --   x = 1
      --   f x = x
      -- :}
      --
      -- The parameter x of f shadows the global value x, so the x in the
      -- body of f must be the bound variable (BName 0), not FName "x".
      testCase "local parameter shadows a global value" $ do
        prog <- convert (Text.unlines ["{:", "  x = 1", "  f x = x", ":}"])
        body <- lookupVal "f" prog
        case body of
          A.Lam _ (A.Var v _) _ -> v @?= A.BName 0
          other -> assertFailure ("unexpected shape: " ++ show other)
    ]

convert :: Text -> IO A.Program
convert src = case Parser.scanAndParse Parser.program "<test>" src of
  Left err -> assertFailure ("parse error: " ++ show err)
  Right concrete ->
    case runExcept (evalStateT (runReaderT (toAbstract concrete) ([], [], False, [])) 0) of
      Left err -> assertFailure ("scope error: " ++ show err)
      Right prog -> return prog

lookupVal :: Text -> A.Program -> IO A.Expr
lookupVal name prog =
  case [e | (n, _, Just e) <- A.vals prog, nameToText n == name] of
    [e] -> return e
    es -> assertFailure ("expected one definition of " ++ show name ++ ", got " ++ show (length es))
