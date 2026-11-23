{-# LANGUAGE OverloadedStrings #-}

module Test.ToAbstractLN (tests) where

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Syntax.AbstractLN as A
import Syntax.Common (nameToText)
import qualified Syntax.Common.BuiltInOp as B
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
          other -> assertFailure ("unexpected shape: " ++ show other),
      -- {:
      --   f 0 y = y
      -- :}
      --
      -- 0 是 refutable pattern，不是單純的 binder，因此 f 會 desugar 成
      -- 兩層 lambda 加 case。
      -- 為了說明 scope，將兩個 lambda 的參數稱為 arg1 和 arg2：
      --
      --   f = \arg1 -> \arg2 ->
      --         case (arg1, arg2) of
      --           (0, y) -> y
      --
      -- BName 依目前位置的 scope，從最靠近的 binder 開始，以 0 編號：
      --   scrutinee (arg1, arg2)：BName 0 = arg2，BName 1 = arg1。
      --   branch body y：BName 0 = pattern 的 y，BName 1 = arg2，
      --                  BName 2 = arg1。
      --
      -- Scrutinee 尚未進入 pattern 的 scope；進入 branch body 時，
      -- pattern 只新增 y 這個 binder，字面值 0 不建立 binder。
      -- 本 test 檢查 scrutinee，因此期望 (BName 1, BName 0)。
      testCase "pattern-matching definition scrutinizes arguments in order" $ do
        prog <- convert (Text.unlines ["{:", "  f 0 y = y", ":}"])
        body <- lookupVal "f" prog
        case body of
          A.Lam _ (A.Lam _ (A.Case scrutinee _ _) _) _ ->
            case scrutinee of
              A.App (A.App (A.Lit (A.Op B.Pr) _) (A.Var v1 _) _) (A.Var v2 _) _ ->
                (v1, v2) @?= (A.BName 1, A.BName 0)
              other -> assertFailure ("unexpected scrutinee: " ++ show other)
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
