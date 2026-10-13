{-# LANGUAGE OverloadedStrings #-}

module Test.Reduce4 (tests) where

import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name (..))
import qualified Syntax.Typed.Reduce4 as Reduce4
import qualified Syntax.Typed.Types as T
import qualified Test.Reduce as Shared
import Test.Reduce.Support (reduciblePathsWith, runWith)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Reduce4"
    [ Shared.tests "shared contract" Reduce4.tryReduceAt Reduce4.redexRT,
      conservativeReferences
    ]

-- Definitions remain opaque until clicked. Intermediate tables intentionally
-- retain updates that older reducers discarded using transitive dependencies.
conservativeReferences :: TestTree
conservativeReferences =
  testGroup
    "conservative reference state"
    [ testCase "an alias chain keeps all updates until its global is exposed" $ do
        let env = [(f, var g), (g, var x)]
            table = [(x, var a), (y, var b)]
            expression = T.Subst (var f) table
            alias = T.Subst (var g) table
            unfolded = T.Subst (var x) table
        paths env expression @?= [[0]]
        run env expression [] @?= Nothing
        run env expression [0] @?= Just alias
        paths env alias @?= [[0]]
        run env alias [0] @?= Just unfolded
        paths env unfolded @?= [[]]
        run env unfolded [] @?= Just (var a),
      testCase "an alias cycle keeps unknown updates without chasing the cycle" $ do
        let env = [(f, var g), (g, var f)]
            expression = T.Subst (var f) [(x, var a)]
        paths env expression @?= [[0]]
        run env expression [] @?= Nothing
        run env expression [0] @?= Just (T.Subst (var g) [(x, var a)]),
      -- f = b does not actually read x. Its caller value is nevertheless
      -- deferred outside x := a because its body has not been unfolded.
      testCase "a caller reference is deferred without querying its global reads" $ do
        let env = [(f, var b)]
            body = T.Subst (T.Tuple [var y, var x]) [(x, var a)]
            expression = T.Case (var f) [T.CaseClause (A.PattBinder y) body] Nothing
        case run env expression [] of
          Just pending@(T.Subst (T.Tuple [T.Var slot _ _, global]) [(key, value), (delayed, argument)]) -> do
            slot @?= delayed
            assertBool "the delayed slot must be fresh" (slot `notElem` [f, x, y, a, b])
            global @?= var x
            (key, value) @?= (x, var a)
            argument @?= var f
            run env pending [] @?= Just (T.Tuple [var f, var a])
            (run env pending [] >>= \distributed -> run env distributed [0])
              @?= Just (T.Tuple [var b, var a])
          result -> assertFailure ("unexpected case result: " <> show result)
    ]
  where
    run = runWith Reduce4.tryReduceAt
    paths = reduciblePathsWith Reduce4.redexRT

f, g, x, y, a, b :: Name
f = Name "f" Nothing
g = Name "g" Nothing
x = Name "x" Nothing
y = Name "y" Nothing
a = Name "a" Nothing
b = Name "b" Nothing

var :: Name -> T.Expr
var name = T.Var name (A.TBase A.TInt Nothing) Nothing
