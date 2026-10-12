{-# LANGUAGE OverloadedStrings #-}

module Test.Reduce3 (tests) where

import Control.Monad (foldM)
import Control.Monad.State (evalState)
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Common (initFreshState)
import GCL.Predicate (PO (..))
import Pretty (toText)
import Server.FileState (FileState (..))
import Server.Load (loadAndDig)
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Typed.Reduce3
import qualified Syntax.Typed.Subst3 as S
import qualified Syntax.Typed.Types as T
import Test.Reduce2 (answersWith, programTests)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Reduce3"
    [ programTests (answersWith run reduciblePaths),
      adversarialScopes,
      smallSteps,
      -- (Inv B)[d \ 5] = (Inv[d \ 5]) B = 5 + x < B.
      -- The untouched global x is metadata, not a displayed identity entry.
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
      -- f = x. An internal Reference with environment [x \ a] must unfold
      -- to x[x \ a], just like the public pending form, without applying it.
      testCase "unfold preserves an internal reference environment" $ do
        let context = S.definitions [(f, var x)]
            reference = S.Reference (var f) [(x, S.resolve context (var a))]
            result = evalState (S.unfold context reference >>= traverse (S.export context)) initFreshState
        result @?= Just (T.Subst (var x) [(x, var a)]),
      -- f = x; g = a; ⟨∀ x : True : (f[x \ a], g, x)⟩. Clicking g
      -- leaves ⟨∀ x' : True : (f[x \ a], a, x')⟩. The displayed state
      -- key x must stay distinct from the binder before f exposes its global.
      testCase "export distinguishes pending state keys from local binders" $ do
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

answers :: [Text] -> [Text]
answers = answersWith run reduciblePaths

-- Keep the real parser/typechecker/WP pipeline, then select the right-hand
-- predicate of the program's single True => Q proof obligation.
loaded :: [Text] -> (DefnEnv, T.Expr)
loaded source = case loadAndDig "<test>" (Text.unlines source) of
  Right (_, Right fs)
    | [po] <- fsProofObligations fs,
      T.App (T.App _ _ _) right _ <- poReducedPred po ->
        (fsDefinitions fs, right)
  _ -> error "the test program must load with exactly one implication"

run :: DefnEnv -> T.Expr -> RedexPath -> Maybe T.Expr
run env expression path = evalState (tryReduceAt env expression path) initFreshState

reduciblePaths :: DefnEnv -> T.Expr -> [RedexPath]
reduciblePaths env expression = collect [] (redexRT env expression)
  where
    collect path (Node reducible children) =
      [path | reducible]
        <> concat [collect (path <> [index]) child | (index, child) <- zip [0 ..] children]

display :: T.Expr -> Text
display = Text.unwords . Text.words . toText

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
