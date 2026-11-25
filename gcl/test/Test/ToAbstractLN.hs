{-# LANGUAGE OverloadedStrings #-}

module Test.ToAbstractLN (tests) where

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Syntax.AbstractLN as A
import Syntax.AbstractLN.Scope (freshIds, openQuant, quant)
import Syntax.Common (nameToText)
import qualified Syntax.Common.BuiltInOp as B
import Syntax.Concrete.Instances.ToAbstractLN (ScopeError (..), toAbstract)
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
          other -> assertFailure ("unexpected shape: " ++ show other),
      -- con N : Int
      -- { case N of _ -> N }
      --
      -- Wildcard _ 不建立 binder，所以 branch body 和 scrutinee 在同一個
      -- scope，兩處的 N 都是 BName 0。
      -- 若把 _ 當成 binder，body 的 N 會變成 BName 1，指向不存在的 binder。
      testCase "wildcard pattern does not shift indices in the branch body" $ do
        prog <- convert (Text.unlines ["con N : Int", "{ case N of _ -> N }"])
        case A.prog prog of
          [A.Assert (A.Case _ [(_, A.Var v _)] _) _] -> v @?= A.BName 0
          other -> assertFailure ("unexpected shape: " ++ show other),
      -- {:
      --   data P = C Int Int
      --   g p = case p of
      --     C _ y -> y
      -- :}
      --
      -- Pattern C _ y 只建立 y 這個 binder，_ 不建立 binder。
      -- BName 依目前位置的 scope，從最靠近的 binder 開始，以 0 編號：
      --   branch body y：BName 0 = pattern 的 y，BName 1 = p。
      -- 因此 body 的 y 必須是 BName 0；若是 BName 1 就會指向 p。
      testCase "wildcard before a binder does not shift the binder's index" $ do
        prog <-
          convert
            (Text.unlines ["{:", "  data P = C Int Int", "  g p = case p of", "    C _ y -> y", ":}"])
        body <- lookupVal "g" prog
        case body of
          A.Lam _ (A.Case _ [(_, A.Var v _)] _) _ -> v @?= A.BName 0
          other -> assertFailure ("unexpected shape: " ++ show other),
      -- {:
      --   data P = C Int Int
      --   g p = case p of
      --     C _ _ -> 0
      -- :}
      --
      -- Wildcard _ 不建立 binder，同一個 pattern 裡的多個 _ 也不算重複的
      -- identifier，因此轉換應該成功，不能報 DuplicatedIdentifiers。
      testCase "multiple wildcards in one pattern are not duplicates" $ do
        prog <-
          convert
            (Text.unlines ["{:", "  data P = C Int Int", "  g p = case p of", "    C _ _ -> 0", ":}"])
        body <- lookupVal "g" prog
        case body of
          A.Lam _ (A.Case _ [(_, A.Lit (A.Num 0) _)] _) _ -> return ()
          other -> assertFailure ("unexpected shape: " ++ show other),
      -- {:
      --   h x x = x
      -- :}
      --
      -- h 的兩個參數都叫 x，是重複的 binder。
      -- 這個定義只有一個 clause，且所有參數 pattern 都是 binder，
      -- 因此直接 desugar 成巢狀 lambda，不需要 case。
      -- 轉換應該報 DuplicatedIdentifiers "x"，而不是讓後面的 x 遮住前面的 x。
      testCase "duplicate parameters are rejected" $ do
        result <- runConvert (Text.unlines ["{:", "  h x x = x", ":}"])
        case result of
          Left (DuplicatedIdentifiers name _) -> name @?= "x"
          other -> assertFailure ("expected DuplicatedIdentifiers, got " ++ show other),
      -- {:
      --   m 0 x x = x
      -- :}
      --
      -- 和上一個例子一樣有重複的 x，但 0 是 refutable pattern，
      -- 因此走 desugar 成 lambda 加 case 的路徑。
      -- 這條路徑也應該報 DuplicatedIdentifiers "x"。
      testCase "duplicate binders beside a refutable pattern are rejected" $ do
        result <- runConvert (Text.unlines ["{:", "  m 0 x x = x", ":}"])
        case result of
          Left (DuplicatedIdentifiers name _) -> name @?= "x"
          other -> assertFailure ("expected DuplicatedIdentifiers, got " ++ show other),
      -- con P, Q : Bool
      -- { P ٭ Q }
      --
      -- ٭ 是 separating conjunction，要轉成 SConj，
      -- 不能變成一般的 conjunction Conj（∧）。
      testCase "separating conjunction stays separating" $ do
        prog <- convert (Text.unlines ["con P, Q : Bool", "{ P ٭ Q }"])
        case A.prog prog of
          [A.Assert (A.App (A.App (A.Lit (A.Op op) _) _ _) _ _) _] -> op @?= B.SConj
          other -> assertFailure ("unexpected shape: " ++ show other),
      -- {:
      --   z = 1
      --   z = 2
      -- :}
      --
      -- z 沒有參數，沒有 pattern 可以用來挑 clause，第二個 clause 永遠用不到。
      -- 這是重複定義，轉換應該報 DuplicatedIdentifiers "z"，而不是 crash。
      testCase "a parameterless value defined twice is rejected" $ do
        result <- runConvert (Text.unlines ["{:", "  z = 1", "  z = 2", ":}"])
        case result of
          Left (DuplicatedIdentifiers name _) -> name @?= "z"
          other -> assertFailure ("expected DuplicatedIdentifiers, got " ++ show other),
      -- {:
      --   m 0 = 1
      --   m x y = 2
      -- :}
      --
      -- m 的第一個 clause 有 1 個參數，第二個有 2 個。
      -- 轉換應該報 ClauseArityMismatch "m"，而不是產生一個 case：
      -- scrutinee 是 pair，第一個 clause 的 pattern 卻只有 0。
      testCase "clauses with different numbers of parameters are rejected" $ do
        result <- runConvert (Text.unlines ["{:", "  m 0 = 1", "  m x y = 2", ":}"])
        case result of
          Left (ClauseArityMismatch name _) -> name @?= "m"
          other -> assertFailure ("expected ClauseArityMismatch, got " ++ show other),
      -- con M : Int
      -- con N : Int
      --
      -- M 和 N 的型別都是 Int，只有在原始碼中的位置（range）不同。
      -- Type 的 Eq 不看 range，所以兩個型別應該相等。
      testCase "the same base type at different positions is equal" $ do
        prog <- convert (Text.unlines ["con M : Int", "con N : Int"])
        case A.consts prog of
          [(_, t1), (_, t2)] -> t1 @?= t2
          other -> assertFailure ("unexpected consts: " ++ show other),
      -- a `arr` b `arr` c 應該是 a → (b → c)，和型別裡的 → 一樣向右結合。
      -- 沒有 fixity 宣告時，Haskell 預設 infixl 9，會變成 (a → b) → c。
      -- builtinTypes 用不加括號的連續 `arr` 寫 kind，例如 TPair 的
      -- TType `arr` TType `arr` TType，要靠這個 fixity 才會是 * → (* → *)。
      testCase "arr associates to the right" $
        (A.TType `A.arr` A.TType `A.arr` A.TType) @?= A.arr A.TType (A.arr A.TType A.TType),
      testCase "freshIds gives distinct names" $ do
        let zs = ["x"]
            ys = freshIds zs ["x", "x0"]
        assertBool ("names collide: " ++ show ys) (nub ys == ys)
        assertBool ("names clash with zs: " ++ show ys) (all (`notElem` zs) ys),
      testCase "openQuant returns the names it opened with" $ do
        let zs = ["x"]
            plus = A.Lit (A.Op B.Add) Nothing
            true = A.Lit (A.Bol True) Nothing
            (_, ys, _, body) = openQuant zs (quant plus ["x"] true (A.fname "x"))
        case ys of
          [y] -> do
            assertBool ("binder clashes with zs: " ++ show y) (y `notElem` zs)
            body @?= A.fname y
          _ -> assertFailure ("expected one binder, got " ++ show ys),
      -- {:
      --   inc x = x + 1
      -- :}
      -- var y : Int
      -- y := inc y        -- 拒絕
      -- { y = inc y }     -- 接受
      --
      -- {: :} 的定義只能在 specification 裡使用，statement 不能用。
      testCase "definitions are usable in specifications but not in statements" $ do
        let defs = ["{:", "  inc x = x + 1", ":}", "var y : Int"]
        result <- runConvert (Text.unlines (defs ++ ["y := inc y"]))
        case result of
          Left (SpecOnlyIdentifier name _) -> name @?= "inc"
          other -> assertFailure ("expected SpecOnlyIdentifier, got " ++ show other)
        _ <- convert (Text.unlines (defs ++ ["{ y = inc y }"]))
        return (),
      -- con a : Int
      -- con M : a
      --
      -- M 的型別 a 是型別變數，不應去查 value 環境裡的 con a，
      -- 應報 TypeNotInScope "a"。
      testCase "type variables do not resolve to value declarations" $ do
        result <- runConvert (Text.unlines ["con a : Int", "con M : a"])
        case result of
          Left (TypeNotInScope name _) -> name @?= "a"
          other -> assertFailure ("expected TypeNotInScope, got " ++ show other),
      -- {:
      --   data P a b = C a b
      -- :}
      --
      -- Constructor 型別裡的 a、b 應在獨立的 type variable scope 解析，
      -- 產生 ∀ a b . BName 1 -> BName 0 -> P (BName 1) (BName 0)。
      testCase "data constructor type parameters resolve in type scope" $ do
        prog <- convert (Text.unlines ["{:", "  data P a b = C a b", ":}"])
        case [t | (n, t) <- A.constrs prog, nameToText n == "C"] of
          [t] ->
            t
              @?= A.tforall
                ["a", "b"]
                ( A.tbname 1
                    `A.arr` A.tbname 0
                    `A.arr` A.tapp (A.tapp (A.tfname "P") (A.tbname 1)) (A.tbname 0)
                )
          other -> assertFailure ("expected one constructor C, got " ++ show other)
    ]

convert :: Text -> IO A.Program
convert src = do
  result <- runConvert src
  case result of
    Left err -> assertFailure ("scope error: " ++ show err)
    Right prog -> return prog

runConvert :: Text -> IO (Either ScopeError A.Program)
runConvert src = case Parser.scanAndParse Parser.program "<test>" src of
  Left err -> assertFailure ("parse error: " ++ show err)
  Right concrete ->
    return (runExcept (evalStateT (runReaderT (toAbstract concrete) ([], [], [], False, [])) 0))

lookupVal :: Text -> A.Program -> IO A.Expr
lookupVal name prog =
  case [e | (n, _, Just e) <- A.vals prog, nameToText n == name] of
    [e] -> return e
    es -> assertFailure ("expected one definition of " ++ show name ++ ", got " ++ show (length es))
