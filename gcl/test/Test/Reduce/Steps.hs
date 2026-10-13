{-# LANGUAGE OverloadedStrings #-}

-- | State, scope and click-granularity behaviour tested only through public
-- reducer operations. No resolved-term or substitution implementation is used.
module Test.Reduce.Steps (tests) where

import Control.Monad (foldM)
import Data.Text (Text)
import Pretty (toText)
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name (..), nameToText)
import qualified Syntax.Typed.Types as T
import Test.Reduce.Support
  ( MarkRedexes,
    ReduceAt,
    answersWith,
    loaded,
    normalForms,
    prettyExpr,
    reduciblePathsWith,
    runWith,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: ReduceAt -> MarkRedexes -> [TestTree]
tests reduce mark =
  [ adversarialScopes,
    smallSteps,
    cancelledStateUpdates,
    -- (Inv B)[d \ 5] = (Inv[d \ 5]) B = 5 + x < B.
    -- No identity entry is displayed for the untouched global x.
    testCase "distributing state keeps only actual updates" $ do
      let (env, expression) =
            loaded
              [ "var d, x : Int",
                "con B : Int",
                "{:",
                "Inv t = d + x < t",
                ":}",
                "{ True }",
                "d := 5",
                "{ Inv B }"
              ]
      case run env expression [] of
        Just distributed@(T.App (T.Subst (T.Var ref _ _) [(key, value)]) _ _) -> do
          nameToText ref @?= "Inv"
          nameToText key @?= "d"
          toText value @?= "5"
          reduciblePaths env distributed @?= [[0, 0]]
          case run env distributed [0, 0] of
            Just result -> display result @?= "5 + x < B"
            Nothing -> assertFailure "the pending definition must unfold"
        result -> assertFailure ("unexpected result: " <> show result),
    -- Inv t = r < t; skip; { case C of r -> Inv B }.
    -- Selecting the case gives Inv B directly, without Inv[r \ r] B.
    testCase "case selection does not introduce identity environments" $ do
      let (env, expression) =
            loaded
              [ "var r : Int",
                "con B, C : Int",
                "{:",
                "Inv t = r < t",
                ":}",
                "{ True }",
                "skip",
                "{ case C of r -> Inv B }"
              ]
      case run env expression [] of
        Just result@(T.App T.Var {} _ _) -> display result @?= "Inv B"
        result -> assertFailure ("unexpected result: " <> show result),
    -- (F 42)[x \ x + 1], F = G, G t = x < t.
    -- Clicking F gives (G 42)[x \ x + 1], with no G[x \ x] wrapper.
    testCase "alias unfolding does not introduce identity environments" $ do
      let (env, expression) =
            loaded
              [ "var x : Int",
                "{:",
                "F = G",
                "G t = x < t",
                ":}",
                "{ True }",
                "x := x + 1",
                "{ F 42 }"
              ]
      case run env expression [0, 0] of
        Just (T.Subst (T.App (T.Var ref _ _) _ _) _) -> nameToText ref @?= "G"
        result -> assertFailure ("unexpected result: " <> show result),
    -- Inv t = B + x < t; (Inv 3)[x \ 1] = (Inv[x \ 1]) 3.
    -- B is read by the definition but has no state update to display.
    testCase "constants are not added to environments" $ do
      let (env, expression) =
            loaded
              [ "var x : Int",
                "con B : Int",
                "{:",
                "Inv t = B + x < t",
                ":}",
                "{ True }",
                "x := 1",
                "{ Inv 3 }"
              ]
      case run env expression [] of
        Just (T.App (T.Subst _ table) _ _) -> map (nameToText . fst) table @?= ["x"]
        result -> assertFailure ("unexpected result: " <> show result),
    -- f = x; f[x \ a] --click f--> x[x \ a] --click Subst--> a.
    testCase "parameterless unfolding leaves its pending environment" $ do
      let env = [(f, var x)]
          expression = T.Subst (var f) [(x, var a)]
          unfolded = T.Subst (var x) [(x, var a)]
      reduciblePaths env expression @?= [[0]]
      run env expression [0] @?= Just unfolded
      reduciblePaths env unfolded @?= [[]]
      run env unfolded [] @?= Just (var a),
    -- f = x; f[x \ x] --click f--> x[x \ x] --click Subst--> x.
    -- Explicit identity operations also retain their own reduction step.
    testCase "an explicit identity table keeps its own click" $ do
      let unfolded = T.Subst (var x) [(x, var x)]
      run [(f, var x)] (T.Subst (var f) [(x, var x)]) [0] @?= Just unfolded
      run [(f, var x)] unfolded [] @?= Just (var x),
    -- f x y = (x, y); f a is partial, while f a b consumes both lambdas.
    testCase "partial applications remain unclickable" $ do
      let body = T.Lam x intType (T.Lam y intType (T.Tuple [var x, var y]) Nothing) Nothing
          function = T.Var f (A.mkArrowType intType (A.mkArrowType intType intType)) Nothing
          partial = T.App function (var a) Nothing
      run [(f, body)] partial [0] @?= Nothing
      run [(f, body)] (T.App partial (var b) Nothing) [0, 0] @?= Just (T.Tuple [var a, var b]),
    -- f = f is intentionally one unfold, even though it has no visible change.
    testCase "self-reference still counts as one unfold" $
      run [(f, var f)] (var f) [] @?= Just (var f),
    -- f y = g x; g y = f x. With x := b, both the call argument and
    -- the next definition reference retain b: (f a)[x \ b] = (g[x \ b]) b.
    testCase "mutual recursion preserves state after unfolding" $ do
      let unary = A.mkArrowType intType intType
          body name = T.Lam y intType (T.App (T.Var name unary Nothing) (var x) Nothing) Nothing
          env = [(f, body g), (g, body f)]
          expression = T.Subst (T.App (T.Var f unary Nothing) (var a) Nothing) [(x, var b)]
      case run env expression [] >>= \distributed -> run env distributed [0, 0] of
        Just (T.App (T.Subst (T.Var ref _ _) [(key, value)]) argument _) -> do
          ref @?= g
          key @?= x
          value @?= var b
          argument @?= var b
        result -> assertFailure ("unexpected result: " <> show result)
  ]
  where
    run = runWith reduce
    reduciblePaths = reduciblePathsWith mark
    answers = answersWith run reduciblePaths

    smallSteps :: TestTree
    smallSteps =
      testGroup
        "small steps except lambda repair"
        [ -- Inv t = d + x < t; (Inv B)[d \ 5]. Clicking Inv removes its
          -- parameter Lam, leaving (d + x < B)[d \ 5] for the next click.
          testCase "beta repair stops before the surrounding state operation" $ do
            let (env, expression) =
                  loaded
                    [ "var d, x : Int",
                      "con B : Int",
                      "{:",
                      "Inv t = d + x < t",
                      ":}",
                      "{ True }",
                      "d := 5",
                      "{ Inv B }"
                    ]
            case run env expression [0, 0] of
              Just unfolded@(T.Subst body _) -> do
                display body @?= "d + x < B"
                case run env unfolded [] of
                  Just result -> display result @?= "5 + x < B"
                  Nothing -> assertFailure "the remaining state operation must be clickable"
              result -> assertFailure ("unexpected result: " <> show result),
          -- Inv t = x < t; ((Inv y)[y \ x])[x \ x + 1]. Clicking Inv
          -- leaves both tables: ((x < y)[y \ x])[x \ x + 1].
          -- Clicking the outer table composes them without rewriting x < y.
          testCase "unfolding leaves every state operation beyond its applications" $ do
            let (env, expression) =
                  loaded
                    [ "var x, y : Int",
                      "{:",
                      "Inv t = x < t",
                      ":}",
                      "{ True }",
                      "x := x + 1",
                      "; y := x",
                      "{ Inv y }"
                    ]
            case run env expression [0, 0, 0] of
              Just unfolded@(T.Subst (T.Subst body _) _) -> do
                display body @?= "x < y"
                case run env unfolded [] of
                  Just composed@(T.Subst original table) -> do
                    original @?= body
                    map (nameToText . fst) table @?= ["y", "x"]
                    map (display . snd) table @?= ["x + 1", "x + 1"]
                    case run env composed [] of
                      Just result -> display result @?= "x + 1 < x + 1"
                      Nothing -> assertFailure "the composed table must be clickable"
                  result -> assertFailure ("unexpected composition: " <> show result)
              result -> assertFailure ("unexpected unfold: " <> show result),
          -- ((x, y)[x \ y])[x, y \ b, a] = (x, y)[x, y \ a, a].
          -- The earlier x entry wins; its RHS reads y through the later table.
          testCase "composition retains the subject and simultaneous values" $ do
            let subject = T.Tuple [var x, var y]
                expression = T.Subst (T.Subst subject [(x, var y)]) [(x, var b), (y, var a)]
                composed = T.Subst subject [(x, var a), (y, var a)]
            run [] expression [] @?= Just composed
            run [] composed [] @?= Just (T.Tuple [var a, var a]),
          -- ((x[x \ a]), y)[y \ b] = (x[x \ a], b).
          -- A pending operation inside a child also survives the outer click.
          testCase "substitution leaves unrelated nested operations pending" $ do
            let inner = T.Subst (var x) [(x, var a)]
                expression = T.Subst (T.Tuple [inner, var y]) [(y, var b)]
                distributed = T.Tuple [inner, var b]
            run [] expression [] @?= Just distributed
            run [] distributed [0] @?= Just (T.Tuple [var a, var b]),
          -- f y = (x, y); (f[x \ a]) x = (a, x).
          -- The environment sits between Lam and App, so repair must consume it.
          -- Applying it after beta would incorrectly replace the argument's x.
          testCase "an interposed environment is consumed before beta" $ do
            let unary = A.mkArrowType intType intType
                body = T.Lam y intType (T.Tuple [var x, var y]) Nothing
                expression = T.App (T.Subst (T.Var f unary Nothing) [(x, var a)]) (var x) Nothing
            run [(f, body)] expression [0, 0] @?= Just (T.Tuple [var a, var x]),
          -- f y z = (x, y, z); ((f[x \ a]) x)[x \ b] x = (a, b, x).
          -- Both Lams require repair: the first argument then sees b, while the
          -- last x belongs to the later call. No standalone Lam is returned.
          testCase "repair crosses environments between multiple applications" $ do
            let binary = A.mkArrowType intType (A.mkArrowType intType intType)
                body = T.Lam y intType (T.Lam a intType (T.Tuple [var x, var y, var a]) Nothing) Nothing
                partial = T.App (T.Subst (T.Var f binary Nothing) [(x, var a)]) (var x) Nothing
                expression = T.App (T.Subst partial [(x, var b)]) (var x) Nothing
            run [(f, body)] expression [0, 0, 0, 0] @?= Just (T.Tuple [var a, var b, var x]),
          -- case x of y -> (y, x)[x \ a]. Selecting the clause leaves a
          -- simultaneous table: (y', x)[x, y' \ a, x], rather than evaluating
          -- x \ a first. The inserted argument x must read the caller's state.
          testCase "case selection preserves a pending operation and its state boundary" $ do
            let body = T.Subst (T.Tuple [var y, var x]) [(x, var a)]
                expression = T.Case (var x) [T.CaseClause (A.PattBinder y) body] Nothing
            case run [] expression [] of
              Just selected@(T.Subst (T.Tuple [T.Var slot _ _, global]) [(key, value), (delayed, argument)]) -> do
                slot @?= delayed
                assertBool "the delayed slot must be fresh" (slot `notElem` [x, y, a])
                global @?= var x
                key @?= x
                value @?= var a
                argument @?= var x
                run [] selected [] @?= Just (T.Tuple [var x, var a])
                -- Reducing the inner table before the case gives the same result.
                (run [] expression [1] >>= \earlier -> run [] earlier []) @?= Just (T.Tuple [var x, var a])
              result -> assertFailure ("unexpected case result: " <> show result),
          -- f = x; case f of y -> (y, x)[x \ a] = (x, a).
          -- f belongs to the caller's state. Unfolding it before selection, in
          -- the delayed table's value, or after that table runs must all agree.
          testCase "a delayed case value keeps a definition in the caller's state" $ do
            let env = [(f, var x)]
                body = T.Subst (T.Tuple [var y, var x]) [(x, var a)]
                expression = T.Case (var f) [T.CaseClause (A.PattBinder y) body] Nothing
            mapM_
              (\clicks -> foldM (run env) expression clicks @?= Just (T.Tuple [var x, var a]))
              [ [[], [], [0]], -- select case, apply state, unfold f
                [[], [2], []], -- select case, unfold f in its delayed value, apply state
                [[0], [], []], -- unfold the scrutinee, select case, apply state
                [[1], [], [0]] -- apply the clause's state, select case, unfold f
              ],
          -- f = x; f[x, y \ a, b] = f[x \ a]. A reference stays a leaf: the
          -- entries it reads wait in a pending Subst, and y, which f
          -- never reads, is dropped.
          testCase "a state table over a reference keeps only the entries it reads" $ do
            let env = [(f, var x)]
                expression = T.Subst (var f) [(x, var a), (y, var b)]
            run env expression [] @?= Just (T.Subst (var f) [(x, var a)]),
          -- f = x; g = a; ⟨∀ x : True : (f[x \ a], g, x)⟩. Clicking g
          -- leaves ⟨∀ x' : True : (f[x \ a], a, x')⟩. The displayed state
          -- key x must stay distinct from the binder before f exposes its global.
          testCase "pending state keys stay distinct from local binders" $ do
            let truth = T.Lit (A.Bol True) (A.TBase A.TBool Nothing) Nothing
                pending = T.Subst (var f) [(x, var a)]
                expression = T.Quant truth [(x, intType)] truth (T.Tuple [pending, var g, var x]) Nothing
            case run [(f, var x), (g, var a)] expression [2, 1] of
              Just (T.Quant _ [(binder, _)] _ (T.Tuple [T.Subst reference [(key, value)], unfolded, bound]) _) -> do
                assertBool "the binder must not hide a state key" (binder /= x)
                reference @?= var f
                key @?= x
                value @?= var a
                unfolded @?= var a
                bound @?= var binder
              result -> assertFailure ("unexpected quantified result: " <> show result)
        ]

    cancelledStateUpdates :: TestTree
    cancelledStateUpdates =
      testGroup
        "cancelled state updates"
        [ -- g = x; f t = g < t. ((f[x \ y])[y \ x]) b --click f-->
          -- g[x \ x] < b --click Subst--> g < b --click g--> x < b.
          -- Lam repair composes the updates but leaves the resulting identity
          -- over g for its own click; it must not silently return g < b.
          testCase "lambda repair leaves a composed identity table clickable" $ do
            let (env, expression) = cancelledCall
            case run env expression [0, 0, 0] of
              Just pending -> do
                display pending @?= "g [ x \\ x ] < b"
                reduciblePaths env pending @?= [[1], [1, 0]]
                case run env pending [1] of
                  Just withoutTable -> do
                    display withoutTable @?= "g < b"
                    case run env withoutTable [1] of
                      Just result -> do
                        display result @?= "x < b"
                        reduciblePaths env result @?= []
                      Nothing -> assertFailure "g must remain available to unfold"
                  Nothing -> assertFailure "the composed identity table must have its own click"
              Nothing -> assertFailure "f must unfold through both environments and its application",
          -- g = x; f t = g < t. After clicking f in ((f[x \ y])[y \ x]) b,
          -- choose g first: g[x \ x] < b -> x[x \ x] < b -> x < b.
          -- Unfolding the parameterless g must also leave the identity pending.
          testCase "unfolding through a composed identity retains the table" $ do
            let (env, expression) = cancelledCall
            case run env expression [0, 0, 0] >>= \pending -> run env pending [1, 0] of
              Just unfolded -> do
                display unfolded @?= "x [ x \\ x ] < b"
                reduciblePaths env unfolded @?= [[1]]
                case run env unfolded [1] of
                  Just result -> display result @?= "x < b"
                  Nothing -> assertFailure "the identity must disappear on its substitution click"
              Nothing -> assertFailure "g must unfold inside the composed identity",
          -- g = x; f t = g < t. ((f[x \ y])[y \ x]) b = x < b.
          -- Compose the tables before unfolding f, or unfold f first and then
          -- choose either g or its identity table: every click order must agree.
          testCase "all click orders preserve the original global after cancellation" $ do
            let (env, expression) = cancelledCall
            normalForms run reduciblePaths env expression @?= ["x < b"],
          -- g = x; (g[x \ y])[y \ x] --click outer Subst--> g[x \ x]
          --   --click identity Subst--> g --click g--> x.
          -- Ordinary composition also retains its subject until a later click.
          testCase "ordinary composition leaves a generated identity pending" $ do
            let env = [(g, var x)]
                expression = T.Subst (T.Subst (var g) [(x, var y)]) [(y, var x)]
                composed = T.Subst (var g) [(x, var x)]
            run env expression [] @?= Just composed
            reduciblePaths env composed @?= [[], [0]]
            run env composed [] @?= Just (var g)
            run env (var g) [] @?= Just (var x)
            normalForms run reduciblePaths env expression @?= ["x"],
          -- g = x; case y of x -> g[x \ x] = y.
          -- The table key is global x, but its RHS is the local case binder x:
          -- this is a real update, not an identity. Clicking the table first
          -- keeps it pending and renames the binder: case y of x' -> g[x \ x'].
          testCase "a same-named local value is not discarded as an identity" $ do
            let env = [(g, var x)]
                body = T.Subst (var g) [(x, var x)]
                expression = T.Case (var y) [T.CaseClause (A.PattBinder x) body] Nothing
            case run env expression [1] of
              Just (T.Case scrutinee [T.CaseClause (A.PattBinder binder) (T.Subst reference [(key, value)])] _) -> do
                scrutinee @?= var y
                assertBool "the binder must stay distinct from the global state key" (binder /= x)
                reference @?= var g
                key @?= x
                value @?= var binder
              result -> assertFailure ("a local replacement must remain pending: " <> show result)
            normalForms run reduciblePaths env expression @?= ["y"]
        ]
      where
        -- GCL declarations/call: g = x; f t = g < t; { True } skip { f b }.
        -- Build the intermediate ((f[x \ y])[y \ x]) b with both operations
        -- between the definition and its application, where Lam repair is needed.
        cancelledCall =
          let (env, call) =
                loaded
                  [ "var x, y, b : Int",
                    "{:",
                    "g = x",
                    "f t = g < t",
                    ":}",
                    "{ True }",
                    "skip",
                    "{ f b }"
                  ]
           in case call of
                T.App function argument location ->
                  (env, T.App (T.Subst (T.Subst function [(x, var y)]) [(y, var x)]) argument location)
                _ -> error "the cancellation fixture must load a definition call"

    adversarialScopes :: TestTree
    adversarialScopes =
      testGroup
        "declaration scope and state composition"
        [ -- Inv t = ⟨∀ x : 0 ≤ x : x < t + d⟩; d := x.
          -- (Inv B)[d \ x] = ⟨∀ x' : 0 ≤ x' : x' < B + x⟩.
          testCase "a replacement avoids a binder inside the definition" $
            answers
              [ "var d, x : Int",
                "con B : Int",
                "{:",
                "Inv t = ⟨ ∀ x : 0 ≤ x : x < t + d ⟩",
                ":}",
                "{ True }",
                "d := x",
                "{ Inv B }"
              ]
              @?= ["True ⇒ ⟨ ∀ $1 : Int : 0 ≤ $1 : $1 < B + x ⟩"],
          -- P = x > 0; Q t = x < t; x, y := y, x.
          -- (P ∧ Q y)[x, y \ y, x] = y > 0 ∧ y < x.
          testCase "two definitions read the same swapped global" $
            answers
              [ "var x, y : Int",
                "{:",
                "P = x > 0",
                "Q t = x < t",
                ":}",
                "{ True }",
                "x, y := y, x",
                "{ P ∧ Q y }"
              ]
              @?= ["True ⇒ y > 0 ∧ y < x"],
          -- K = x + y; y := K; x := y; { ⟨∀ y : True : x = y⟩ }.
          -- The local y stays bound; the x and y in the earlier RHS stay global:
          -- (⟨∀ y : True : x = y⟩[x \ y])[y \ K]
          --   = ⟨∀ y' : True : K = y'⟩ = ⟨∀ y' : True : x + y = y'⟩.
          testCase "an earlier RHS stays outside the postcondition's binder" $
            answers
              [ "var x, y : Int",
                "{:",
                "K = x + y",
                ":}",
                "{ True }",
                "y := K",
                "; x := y",
                "{ ⟨ ∀ y : True : x = y ⟩ }"
              ]
              @?= ["True ⇒ ⟨ ∀ $1 : Int : True : x + y = $1 ⟩"],
          -- K = x; x := x + 1; { ⟨∀ x : 0 ≤ x : ⟨∀ x : True : K = x⟩⟩ }.
          -- Both binders stay local; only K's global is incremented:
          -- ⟨∀ x' : 0 ≤ x' : ⟨∀ x'' : True : x + 1 = x''⟩⟩.
          testCase "nested same-named binders do not hide a global update" $
            answers
              [ "var x : Int",
                "{:",
                "K = x",
                ":}",
                "{ True }",
                "x := x + 1",
                "{ ⟨ ∀ x : 0 ≤ x : ⟨ ∀ x : True : K = x ⟩ ⟩ }"
              ]
              @?= ["True ⇒ ⟨ ∀ $1 : Int : 0 ≤ $1 : ⟨ ∀ $2 : Int : True : x + 1 = $2 ⟩ ⟩"],
          -- F = G; G = K; K t = r < t; r := r + 1.
          -- (case C of r -> F r)[r \ r + 1] = r + 1 < C.
          testCase "state crosses an alias chain and a case binder" $
            answers
              [ "var r : Int",
                "con C : Int",
                "{:",
                "F = G",
                "G = K",
                "K t = r < t",
                ":}",
                "{ True }",
                "r := r + 1",
                "{ case C of r -> F r }"
              ]
              @?= ["True ⇒ r + 1 < C"],
          -- con c : Int; F t = c < t; { ⟨∀ c : True : F c⟩ }.
          -- The declaration's c must stay free even though no assignment can update it:
          -- ⟨∀ c' : True : c < c'⟩.
          testCase "a constant keeps declaration scope under a same-named binder" $
            answers
              [ "con c : Int",
                "{:",
                "F t = c < t",
                ":}",
                "{ True }",
                "skip",
                "{ ⟨ ∀ c : True : F c ⟩ }"
              ]
              @?= ["True ⇒ ⟨ ∀ $1 : Int : True : c < $1 ⟩"]
        ]

display :: T.Expr -> Text
display = prettyExpr

f, g, x, y, a, b :: Name
f = Name "f" Nothing
g = Name "g" Nothing
x = Name "x" Nothing
y = Name "y" Nothing
a = Name "a" Nothing
b = Name "b" Nothing

intType :: A.Type
intType = A.TBase A.TInt Nothing

var :: Name -> T.Expr
var name = T.Var name intType Nothing
