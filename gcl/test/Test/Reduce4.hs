{-# LANGUAGE OverloadedStrings #-}

module Test.Reduce4 (tests) where

import Data.Bifunctor (bimap)
import qualified Data.Text as Text
import GCL.Predicate (PO (..))
import GCL.Range (mkPos, mkRange)
import Server.FileState (FileState (..))
import Server.Load (loadAndDig)
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name (..))
import qualified Syntax.Typed.Reduce4 as Reduce4
import qualified Syntax.Typed.Types as T
import qualified Test.Reduce as Shared
import Test.Reduce.Support (loaded, normalFormText, normalForms, prettyExpr, reduciblePathsWith, runWith)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Reduce4"
    [ Shared.tests "shared contract" Reduce4.tryReduceAt Reduce4.redexRT,
      fullReduction,
      conservativeReferences,
      pendingHoles
    ]

fullReduction :: TestTree
fullReduction =
  testGroup
    "reduceAll"
    [ testCase "a normal form succeeds without spending any steps" $
        mapM_ (\limit -> Reduce4.reduceAll limit [] (var x) @?= Right (var x)) [-1, 0, 1],
      testCase "a non-positive limit leaves a reducible input unfinished" $
        mapM_ (\limit -> Reduce4.reduceAll limit [(f, var x)] (var f) @?= Left (var f)) [-1, 0],
      testCase "the limit returns the intermediate alias with its pending update" $ do
        let env = [(f, var g), (g, var x)]
            expression = T.Subst (var f) [(x, var a)]
        Reduce4.reduceAll 1 env expression @?= Left (T.Subst (var g) [(x, var a)])
        Reduce4.reduceAll 2 env expression @?= Left (T.Subst (var x) [(x, var a)])
        Reduce4.reduceAll 3 env expression @?= Right (var a)
        Reduce4.reduceAll 4 env expression @?= Right (var a),
      testCase "children are reduced in path order" $ do
        let env = [(f, var x), (g, var y)]
            expression = T.Tuple [var f, var g]
        Reduce4.reduceAll 1 env expression @?= Left (T.Tuple [var x, var g])
        Reduce4.reduceAll 2 env expression @?= Right (T.Tuple [var x, var y]),
      testCase "a root projection discards an unused recursive definition" $
        Reduce4.reduceAll 1 [(f, var f)] (T.OutT 0 (T.Tuple [var x, var f]))
          @?= Right (var x),
      testCase "a self-reference spends its budget even when its text is unchanged" $
        Reduce4.reduceAll 5 [(f, var f)] (var f) @?= Left (var f),
      testCase "an alias cycle stops at the requested number of unfolds" $
        Reduce4.reduceAll 3 [(f, var g), (g, var f)] (var f) @?= Left (var g),
      -- K = x; x := 1 { ⟨∀ x : True : K = x⟩ }. While K [ x \ 1 ] or
      -- x [ x \ 1 ] is pending, a displayed binder x would capture its state
      -- key, so partial results use a fallback name. It skips only names that
      -- remain, not private names of earlier steps. Once the key is gone, the
      -- result returns to the input's x instead of that fallback.
      testCase "the result restores binder names only an intermediate step had to avoid" $ do
        let (env, start) = loadOne ["var x : Int", "{:", "  K = x", ":}", "{ True }", "x := 1", "{ ⟨ ∀ x : True : K = x ⟩ }"]
            result limit = bimap prettyExpr prettyExpr (Reduce4.reduceAll limit env start)
        result 1 @?= Left "True ⇒ ⟨ ∀ ?x_0 : Int : True : K [ x \\ 1 ] = ?x_0 ⟩"
        result 2 @?= Left "True ⇒ ⟨ ∀ ?x_0 : Int : True : x [ x \\ 1 ] = ?x_0 ⟩"
        result 3 @?= Right "True ⇒ ⟨ ∀ x : Int : True : 1 = x ⟩",
      -- F t = ⟨∀ x : 0 ≤ x : t < x⟩; x := x + 1 { F x }. The argument reads the
      -- global x inside the binder, so even the result needs a fallback name.
      -- Its number restarts after the steps instead of counting their binders.
      testCase "an unavoidable fallback name does not grow with the steps taken" $ do
        let (env, start) = loadOne ["var x : Int", "{:", "  F t = ⟨ ∀ x : 0 ≤ x : t < x ⟩", ":}", "x := x + 1", "{ F x }"]
        fmap prettyExpr (Reduce4.reduceAll 1000 env start)
          @?= Right "True ⇒ ⟨ ∀ ?x_0 : Int : 0 ≤ ?x_0 : x + 1 < ?x_0 ⟩",
      -- f = x; case b of 0 -> (a, a); x -> (f, x). Unfolding f puts the global x
      -- under the clause binder, which therefore needs a fallback name. That
      -- name must avoid a free ?x_0 in its scope, which it would capture, and
      -- one elsewhere in the result, which would merely look like it.
      testCase "a fallback name avoids private-looking names already in the result" $ do
        let taken = Name "?x_0" Nothing
            -- b is symbolic, so the first clause stays undecided and blocks the second.
            clause elements =
              T.Case
                (var b)
                [ T.CaseClause (A.PattLit (A.Num 0)) (T.Tuple (map (const (var a)) elements)),
                  T.CaseClause (A.PattBinder x) (T.Tuple elements)
                ]
                Nothing
            fallback binder = assertBool ("unexpected fallback name: " <> show binder) (binder `notElem` [x, taken])
        case Reduce4.reduceAll 10 [(f, var x)] (clause [var f, var taken, var x]) of
          Right (T.Case _ [_, T.CaseClause (A.PattBinder binder) body] _) -> do
            fallback binder
            body @?= T.Tuple [var x, var taken, var binder]
          result -> assertFailure ("unexpected result inside the scope: " <> show result)
        case Reduce4.reduceAll 10 [(f, var x)] (T.Tuple [var taken, clause [var f, var x]]) of
          Right (T.Tuple [free, T.Case _ [_, T.CaseClause (A.PattBinder binder) body] _]) -> do
            fallback binder
            free @?= var taken
            body @?= T.Tuple [var x, var binder]
          result -> assertFailure ("unexpected result outside the scope: " <> show result),
      Shared.programTests answers
    ]
  where
    answers source =
      let (env, start) = loadOne source
       in case Reduce4.reduceAll 1000 env start of
            Right result
              | null (reduciblePathsWith Reduce4.redexRT env result) -> [normalFormText result]
              | otherwise -> error "reduceAll returned Right with a remaining redex"
            Left partial -> error ("reduceAll exhausted its budget: " <> Text.unpack (prettyExpr partial))
    loadOne source = case loadAndDig "<test>" (Text.unlines source) of
      Right (_, Right fs) | [po] <- fsProofObligations fs -> (fsDefinitions fs, poReducedPred po)
      _ -> error "the test program must load with exactly one proof obligation"

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

pendingHoles :: TestTree
pendingHoles =
  testGroup
    "pending hole state"
    [ testCase "state propagation preserves a hole and its source metadata" $ do
        let expression = T.Subst (T.Tuple [hole, var x]) [(x, var a), (y, var y)]
            expected = T.Tuple [T.Subst hole [(x, var a)], var a]
        run [] expression [] @?= Just expected
        paths [] expected @?= [],
      testCase "a pending hole table waits for refinement" $ do
        let pending = T.Subst hole [(x, var a)]
        run [] pending [] @?= Nothing
        paths [] pending @?= []
        Reduce4.reduceAll 0 [] pending @?= Right pending,
      testCase "empty and identity tables can still be removed from a hole" $
        mapM_
          ( \table -> do
              let expression = T.Subst hole table
              paths [] expression @?= [[]]
              run [] expression [] @?= Just hole
          )
          [[], [(x, var x)]],
      testCase "a pending hole table can reduce its replacement expressions" $ do
        let env = [(f, var x)]
            expression = T.Subst hole [(x, var f)]
            identity = T.Subst hole [(x, var x)]
        paths env expression @?= [[1]]
        run env expression [1] @?= Just identity
        run env identity [] @?= Just hole,
      testCase "composition retains outer updates that a hole may read" $ do
        let expression = T.Subst (T.Subst hole [(y, var b)]) [(x, var a)]
            expected = T.Subst hole [(y, var b), (x, var a)]
        run [] expression [] @?= Just expected
        paths [] expected @?= []
        Reduce4.reduceAll 1 [] expression @?= Right expected,
      -- x := x + 1; y := ? { y = x }. The hole is evaluated after x's update;
      -- reducing either table first must keep that update pending over it.
      testCase "all click orders preserve the state of an assignment hole" $ do
        let (env, expression) = loaded ["var x, y : Int", "{ True }", "x := x + 1", "y := ?", "{ y = x }"]
            expected = "{! !}₀ [ x \\ x + 1 ] = x + 1"
        normalForms run paths env expression @?= [expected]
        fmap prettyExpr (Reduce4.reduceAll 100 env expression) @?= Right expected
    ]
  where
    run = runWith Reduce4.tryReduceAt
    paths = reduciblePathsWith Reduce4.redexRT
    hole = T.EHole (T.Hole "hole" 0 (A.TBase A.TInt Nothing) (mkRange (mkPos 1 1) (mkPos 1 2)) mempty)

f, g, x, y, a, b :: Name
f = Name "f" Nothing
g = Name "g" Nothing
x = Name "x" Nothing
y = Name "y" Nothing
a = Name "a" Nothing
b = Name "b" Nothing

var :: Name -> T.Expr
var name = T.Var name (A.TBase A.TInt Nothing) Nothing
