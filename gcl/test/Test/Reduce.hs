{-# LANGUAGE OverloadedStrings #-}

-- | Reducer behaviour shared by implementations of tryReduceAt and redexRT.
module Test.Reduce (tests, basicTests, programTests) where

import Control.Monad.State (State, evalState)
import Data.Maybe (isJust)
import Data.Text (Text)
import GCL.Common (initFreshState)
import GCL.Range (Range, mkPos, mkRange)
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (ArithOp (..), ChainOp (LTE), Name (..), Op (..))
import Syntax.Typed.Structure
import qualified Syntax.Typed.Types as T
import qualified Test.Reduce.Steps as Steps
import Test.Reduce.Support
  ( DefnEnv,
    MarkRedexes,
    ReduceAt,
    answersWith,
    reduciblePathsWith,
    runWith,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | The complete current contract, including individual state-operation clicks.
-- Older reducers can still run basicTests and programTests separately.
tests :: String -> ReduceAt -> MarkRedexes -> TestTree
tests name reduce mark =
  testGroup name $
    [ basicTests reduce mark,
      programTests (answersWith (runWith reduce) (reduciblePathsWith mark))
    ]
      <> Steps.tests reduce mark

-- | These GCL/AST sketches define reducer behaviour independently of its
-- implementation. Reducers must execute the same basic rules, respect
-- scopes and paths, and preserve the lambda-free presentation contract.
basicTests :: ReduceAt -> MarkRedexes -> TestTree
basicTests reduce mark =
  testGroup
    "basic reduction"
    [ -- F = G; G x y = (x, y). Clicking F leaves G for a later click.
      testCase "an alias unfolds exactly once" $
        runCase aliasDefnEnv (var f pairFunctionType) [] @?= Just (var g pairFunctionType),
      -- F = F. Clicking F is one unfold even though its spelling is unchanged.
      testCase "a self-reference still counts as an unfold step" $
        runCase [(f, var f pairFunctionType)] (var f pairFunctionType) []
          @?= Just (var f pairFunctionType),
      -- G x y = (x, y); G a b --click G--> (a, b), consuming both Lams.
      testCase "a fully applied definition repairs every leading lambda" $
        runCase binaryDefnEnv fullCall [0, 0] @?= Just (T.Tuple [var a intType, var b intType]),
      -- H x = x; H a --click H--> a, consuming its single Lam.
      testCase "a one-parameter definition consumes its single application" $
        runCase unaryDefnEnv unaryCall [0] @?= Just (var a intType),
      -- G x y = (x, y); G a cannot unfold while its second Lam would remain.
      testCase "a partial application is not clickable" $
        runCase binaryDefnEnv partialCall [0] @?= Nothing,
      -- G x y = (x, y); h G. Clicking the argument G cannot use h's App.
      testCase "an argument cannot borrow its parent application" $
        runCase binaryDefnEnv argumentCall [1] @?= Nothing,
      -- AST sketch: OutT 1 (a, b) = b.
      testCase "tuple projection reduces locally" $
        runCase [] (T.OutT 1 (T.Tuple [var a intType, var b intType])) []
          @?= Just (var b intType),
      -- (x, y)[x \ a] = (a, y).
      testCase "an explicit substitution reduces locally" $
        runCase [] (T.Subst (T.Tuple [var x intType, var y intType]) [(x, var a intType)]) []
          @?= Just (T.Tuple [var a intType, var y intType]),
      -- case True of False -> a; x -> b = b, skipping the false literal.
      testCase "case skips a refuted clause" $
        runCase
          []
          ( T.Case
              truth
              [ T.CaseClause (A.PattLit (A.Bol False)) (var a intType),
                T.CaseClause (A.PattBinder x) (var b intType)
              ]
              Nothing
          )
          []
          @?= Just (var b intType),
      -- case x of True -> a; y -> b stays pending while x is symbolic.
      testCase "an undecidable case clause blocks later clauses" $
        runCase
          []
          ( T.Case
              (var x boolType)
              [ T.CaseClause (A.PattLit (A.Bol True)) (var a intType),
                T.CaseClause (A.PattBinder y) (var b intType)
              ]
              Nothing
          )
          []
          @?= Nothing,
      -- G x y = (x, y); G a b marks only G at [0,0]. Every path in
      -- the remaining sketches must agree with execution, including failures.
      testCase "redex discovery and execution use the same judgement" $ do
        pathsFor binaryDefnEnv fullCall @?= [[0, 0]]
        mapM_
          ( \(env, expression) ->
              mapM_
                ( \path ->
                    assertEqual
                      ("discovery at " <> show path <> " in " <> show expression)
                      (isJust (runCase env expression path))
                      (path `elem` pathsFor env expression)
                )
                (allPaths expression)
          )
          reductionCases,
      -- The sketches in reductionCases include <forall i : True : (i, F)>
      -- with F = i; fresh-name seeds may change i', but never clickability.
      testCase "clickability is independent of initial fresh-name state" $
        concatMap (stateDependentPaths reduce) reductionCases @?= [],
      -- G x y = (x, y); <forall G : True : G> binds G locally.
      testCase "a lexical binder hides a same-named definition" $
        runCase binaryDefnEnv shadowed [2] @?= Nothing,
      -- F = i; <forall i : True : (i, F)> --click F-->
      -- <forall i' : True : (i', i)>; only the local i is renamed.
      testCase "unfolding through an ancestor binder avoids capture" $
        case runCase [(f, var i intType)] captureCandidate [2, 1] of
          Just (T.Quant _ [(binder, _)] _ (T.Tuple [bound, inserted]) _) -> do
            binder /= i @? "the quantifier binder must be renamed"
            bound @?= var binder intType
            inserted @?= var i intType
          result -> assertFailure ("unexpected result: " <> show result),
      -- (x, f)[x \ a]      with a definition also named x
      --
      -- The x in the subject is the variable the table replaces, not a
      -- reference to the definition, so it is not clickable.
      testCase "a substitution table hides a same-named definition" $
        runCase [(x, var a intType)] pendingSubst [0, 0] @?= Nothing,
      -- f = x, where x is the global the table replaces.
      --
      -- (x, f)[x \ a]
      --   = (x, x)[x \ a]      the table captures the unfolded x
      --   = (a, a)
      --
      -- A table is not a lexical binder: it names the program variable the
      -- assignment updates, which is the same x a definition body refers to.
      testCase "unfolding under a substitution table lets the table capture" $
        runCase [(f, var x intType)] pendingSubst [0, 1]
          @?= Just (T.Subst (T.Tuple [var x intType, var x intType]) [(x, var a intType)]),
      -- Internal-AST example from this GCL program:
      --
      --   var x : Int
      --   con a : Int
      --   {: f = x :}
      --   { True }
      --   x := a
      --   { f = 0 }
      --
      -- After distributing the assignment, the selected PO subexpression is:
      --   f[x \ a] = x[x \ a] = a
      -- Only f is clickable initially; its environment waits for unfolding.
      testCase "a pending definition environment unfolds through its reference" $ do
        let env = [(f, var x intType)]
            expression = T.Subst (var f intType) [(x, var a intType)]
        pathsFor env expression @?= [[0]]
        case runCase env expression [0] of
          Just unfolded -> runCase env unfolded [] @?= Just (var a intType)
          Nothing -> assertFailure "the pending definition must unfold",
      -- Internal-AST example from this GCL program:
      --
      --   var x : Int
      --   con a, b : Int
      --   {:
      --     f : Int -> Int
      --     f y = g x
      --     g : Int -> Int
      --     g y = f x
      --   :}
      --   { True }
      --   x := b
      --   { f a = 0 }
      --
      -- (f a)[x \ b]
      --   = (f[x \ b]) a
      --   = (g[x \ b]) b       unfold f and consume its environment and parameter
      --
      -- The two global x occurrences have different source ranges. This step
      -- must terminate despite those ranges and mutual recursion.
      -- Check just these steps: recursive unfolding has no normal form here.
      testCase "mutually recursive definitions retain the assigned global" $ do
        let occurrence range = T.Var x intType (Just range)
            body name range = T.Lam y intType (T.App (var name unaryType) (occurrence range) Nothing) Nothing
            env = [(f, body g holeRange), (g, body f (mkRange (mkPos 2 1) (mkPos 2 2)))]
            expression = T.Subst (T.App (var f unaryType) (var a intType) Nothing) [(x, var b intType)]
        case runCase env expression [] >>= \distributed -> runCase env distributed [0, 0] of
          Just (T.App (T.Subst reference [(key, value)]) argument _) -> do
            reference @?= var g unaryType
            key @?= x
            value @?= var b intType
            argument @?= var b intType
          result -> assertFailure ("unexpected result: " <> show result),
      -- Each reductionCases sketch starts lambda-free; its definitions may
      -- have a leading Lam prefix. Every marked click must consume exposed
      -- lambdas before returning an expression to the UI.
      testCase "every reducible path yields a lambda-free result" $
        concatMap (lambdasLeft runCase pathsFor) reductionCases @?= [],
      -- 'pathChildren' and 'withPathChildren' are two hand-written lists that
      -- have to agree on every constructor, and where they drift
      -- 'withPathChildren' throws at run time rather than failing to compile.
      -- The children in each sample are distinct, so an ordering drift also
      -- changes the rebuilt expression.
      testCase "withPathChildren rebuilds every constructor unchanged" $
        filter (\e -> withPathChildren e (pathChildren e) /= e) everyConstructor @?= [],
      -- Local reduction leaves same-named binders and existing generated-looking
      -- globals unchanged. Internal hygiene must not alter their displayed names.
      testCase "local reduction preserves unrelated binder names" $ do
        let global = var (Name "?x_0" Nothing) intType
            inner body = T.Quant truth [(x, intType)] truth (T.Tuple [var x intType, body]) Nothing
            outer body = T.Quant truth [(x, intType)] truth (T.Tuple [var x intType, inner body]) Nothing
            expression = outer (T.OutT 0 (T.Tuple [global, var b intType]))
        runCase [] expression [2, 1, 2, 1] @?= Just (outer global),
      -- G x y = (x, y); G a b has no child 7, negative child, or child
      -- below the leaf G. Every such path returns Nothing.
      testCase "invalid paths are rejected" $
        mapM_
          (\path -> runCase binaryDefnEnv fullCall path @?= Nothing)
          [[7], [-1], [0, 7], [0, -1], [0, 0, 0]],
      -- con i : Int; F = i; <forall i : True : (i, F)> --click F-->
      -- <forall i' : True : (i', i)>. Renaming changes spelling, while
      -- the binder, bound occurrence and inserted definition keep their own
      -- Name ranges, Expr ranges and types; Eq Name alone ignores ranges.
      testCase "unfolding and ancestor renaming preserve source metadata" $ do
        let binderRange = Just (mkRange (mkPos 3 1) (mkPos 3 2))
            occurrenceRange = Just (mkRange (mkPos 3 4) (mkPos 3 5))
            boundRange = Just (mkRange (mkPos 3 3) (mkPos 3 6))
            definitionNameRange = Just (mkRange (mkPos 1 5) (mkPos 1 6))
            definitionRange = Just (mkRange (mkPos 1 4) (mkPos 1 7))
            quantifierRange = Just (mkRange (mkPos 3 1) (mkPos 3 20))
            body = T.Const (Name "i" definitionNameRange) intType definitionRange
            bound = T.Var (Name "i" occurrenceRange) intType boundRange
            expression =
              T.Quant
                truth
                [(Name "i" binderRange, intType)]
                truth
                (T.Tuple [bound, var f intType])
                quantifierRange
        case runCase [(f, body)] expression [2, 1] of
          Just
            ( T.Quant
                operator
                [(Name binder binderLocation, binderType)]
                restriction
                ( T.Tuple
                    [ T.Var (Name local localLocation) localType localRange,
                      T.Const (Name global globalLocation) globalType globalRange
                      ]
                  )
                location
              ) -> do
              assertBool "the local binder must be renamed" (binder /= "i")
              local @?= binder
              global @?= "i"
              (binderLocation, localLocation, globalLocation) @?= (binderRange, occurrenceRange, definitionNameRange)
              (localRange, globalRange, location) @?= (boundRange, definitionRange, quantifierRange)
              (binderType, localType, globalType) @?= (intType, intType, intType)
              (operator, restriction) @?= (truth, truth)
          result -> assertFailure ("unexpected source metadata: " <> show result),
      -- con a : Int; (h x)[x \ a] = h a. The untouched App/function keep
      -- their source locations; the replacement brings its own locations.
      testCase "state substitution preserves headers and replacement metadata" $ do
        let functionNameRange = Just (mkRange (mkPos 2 1) (mkPos 2 2))
            functionRange = Just (mkRange (mkPos 2 1) (mkPos 2 3))
            applicationRange = Just (mkRange (mkPos 2 1) (mkPos 2 5))
            replacementNameRange = Just (mkRange (mkPos 1 1) (mkPos 1 2))
            replacementRange = Just (mkRange (mkPos 1 1) (mkPos 1 3))
            function = T.Var (Name "h" functionNameRange) unaryType functionRange
            value = T.Const (Name "a" replacementNameRange) intType replacementRange
            expression = T.Subst (T.App function (var x intType) applicationRange) [(x, value)]
        case runCase [] expression [] of
          Just
            ( T.App
                (T.Var (Name functionName functionLocation) functionType originalRange)
                (T.Const (Name replacementName replacementLocation) replacementType valueRange)
                location
              ) -> do
              (functionName, replacementName) @?= ("h", "a")
              (functionLocation, originalRange, location) @?= (functionNameRange, functionRange, applicationRange)
              (replacementLocation, valueRange) @?= (replacementNameRange, replacementRange)
              (functionType, replacementType) @?= (unaryType, intType)
          result -> assertFailure ("unexpected replacement metadata: " <> show result)
    ]
  where
    runCase env expression path = evalState (reduce env expression path) initFreshState
    pathsFor = reduciblePathsWith mark

-- | Each case is a whole GCL program with one proof obligation. 'answersWith'
--   clicks every redex in every order, so a single expected answer also
--   asserts that the click order does not matter.
--
--   An assignment @v := e@ leaves @Q[v \ e]@ in the obligation.  A definition
--   body's free names are the program's globals, so the pending substitution
--   must reach them once the definition is unfolded.
-- | Run the same GCL source and expected normal forms against any reducer.
programTests :: ([Text] -> [Text]) -> TestTree
programTests solve = testGroup "definition substitution" [substitutionReachesDefinitionBodies solve, bindersStillAvoidCapture solve]

substitutionReachesDefinitionBodies :: ([Text] -> [Text]) -> TestTree
substitutionReachesDefinitionBodies solve =
  testGroup
    "a substitution reaches the globals inside a definition body"
    [ -- (Inv B)[d \ 5]
      --   = (d + x < B)[d \ 5]
      --   = 5 + x < B
      testCase "a single assignment" $
        solve
          [ "var d, x : Int",
            "con B : Int",
            "{:",
            "  Inv t = d + x < t",
            ":}",
            "{ True }",
            "d := 5",
            "{ Inv B }"
          ]
          @?= ["True ⇒ 5 + x < B"],
      -- Issue 179.
      --
      -- (True ∧ Inv B)[r \ r + 1]
      --   = (True ∧ A ≤ r < B)[r \ r + 1]
      --   = True ∧ A ≤ r + 1 < B
      testCase "issue 179" $
        solve
          [ "con A, B : Int",
            "var r : Int",
            "{:",
            "  Inv t = A ≤ r < t",
            ":}",
            "{ True }",
            "r := r + 1",
            "{ True ∧ Inv B }"
          ]
          @?= ["True ⇒ True ∧ A ≤ r + 1 < B"],
      -- K[x \ x + 1]
      --   = (x > 0)[x \ x + 1]
      --   = x + 1 > 0
      testCase "a definition without parameters" $
        solve
          [ "var x : Int",
            "{:",
            "  K = x > 0",
            ":}",
            "{ True }",
            "x := x + 1",
            "{ K }"
          ]
          @?= ["True ⇒ x + 1 > 0"],
      -- (F 42)[x \ x + 1]
      --   = (G 42)[x \ x + 1]
      --   = (x < 42)[x \ x + 1]
      --   = x + 1 < 42
      testCase "an alias of a definition" $
        solve
          [ "var x : Int",
            "{:",
            "  F = G",
            "  G t = x < t",
            ":}",
            "{ True }",
            "x := x + 1",
            "{ F 42 }"
          ]
          @?= ["True ⇒ x + 1 < 42"],
      -- F's parameter x is local; G's x is the global.
      --
      -- (F B)[x \ 1]
      --   = (G B)[x \ 1]
      --   = (x < B)[x \ 1]
      --   = 1 < B
      testCase "a wrapper whose parameter shares the global's name" $
        solve
          [ "var x : Int",
            "con B : Int",
            "{:",
            "  F x = G x",
            "  G t = x < t",
            ":}",
            "{ True }",
            "x := 1",
            "{ F B }"
          ]
          @?= ["True ⇒ 1 < B"],
      -- ((Inv y)[y \ x])[x \ x + 1]
      --   = ((x < y)[y \ x])[x \ x + 1]
      --   = (x < x)[x \ x + 1]
      --   = x + 1 < x + 1
      testCase "two sequential assignments" $
        solve
          [ "var x, y : Int",
            "{:",
            "  Inv t = x < t",
            ":}",
            "{ True }",
            "x := x + 1",
            "; y := x",
            "{ Inv y }"
          ]
          @?= ["True ⇒ x + 1 < x + 1"],
      -- (Inv B)[x, y \ y, x]
      --   = (x < y + B)[x, y \ y, x]
      --   = y < x + B
      testCase "a simultaneous assignment" $
        solve
          [ "var x, y : Int",
            "con B : Int",
            "{:",
            "  Inv t = x < y + t",
            ":}",
            "{ True }",
            "x, y := y, x",
            "{ Inv B }"
          ]
          @?= ["True ⇒ y < x + B"],
      -- F = K, K = x. The later increment reads the already incremented x.
      --
      -- ((F = 0)[x \ x + 2])[x \ x + 1]
      --   = ((x = 0)[x \ x + 2])[x \ x + 1]
      --   = (x + 2 = 0)[x \ x + 1]
      --   = x + 1 + 2 = 0
      testCase "repeated assignments update an alias in execution order" $
        solve
          [ "var x : Int",
            "{:",
            "  F = K",
            "  K = x",
            ":}",
            "{ True }",
            "x := x + 1",
            "; x := x + 2",
            "{ F = 0 }"
          ]
          @?= ["True ⇒ x + 1 + 2 = 0"],
      -- The case binder r is local; Inv's r is the global, which the
      -- assignment replaces.
      --
      -- (case C of r → Inv B)[r \ 7]
      --   = (case C of r' → r < B)[r \ 7]
      --   = case C of r' → 7 < B
      --   = 7 < B
      testCase "a case binder between the assignment and the definition" $
        solve
          [ "var r : Int",
            "con B, C : Int",
            "{:",
            "  Inv t = r < t",
            ":}",
            "{ True }",
            "r := 7",
            "{ case C of r -> Inv B }"
          ]
          @?= ["True ⇒ 7 < B"],
      -- The bound x is passed to Inv; Inv's own x is the global.
      --
      -- ⟨∀ x : True : Inv x⟩[x \ 1]
      --   = ⟨∀ x' : True : x < x'⟩[x \ 1]
      --   = ⟨∀ x' : True : 1 < x'⟩
      testCase "a quantifier binder between the assignment and the definition" $
        solve
          [ "var x : Int",
            "{:",
            "  Inv t = x < t",
            ":}",
            "{ True }",
            "x := 1",
            "{ ⟨ ∀ x : True : Inv x ⟩ }"
          ]
          @?= ["True ⇒ ⟨ ∀ $1 : Int : True : 1 < $1 ⟩"],
      -- The same shape at type Bool.
      --
      -- ⟨∀ r : True : Inv r⟩[r \ True]
      --   = ⟨∀ r' : True : r && r'⟩[r \ True]
      --   = ⟨∀ r' : True : True && r'⟩
      testCase "a quantifier binder over a Bool global" $
        solve
          [ "var r : Bool",
            "{:",
            "  Inv t = r && t",
            ":}",
            "{ True }",
            "r := True",
            "{ ⟨ ∀ r : True : Inv r ⟩ }"
          ]
          @?= ["True ⇒ ⟨ ∀ $1 : Bool : True : True && $1 ⟩"],
      -- The range mentions x directly; the body mentions it through Inv.
      --
      -- ⟨∀ i : 0 ≤ i < x : Inv i⟩[x \ x + 1]
      --   = ⟨∀ i : 0 ≤ i < x : x < i⟩[x \ x + 1]
      --   = ⟨∀ i : 0 ≤ i < x + 1 : x + 1 < i⟩
      testCase "a quantifier whose body reaches the global only through a definition" $
        solve
          [ "var x : Int",
            "{:",
            "  Inv t = x < t",
            ":}",
            "{ True }",
            "x := x + 1",
            "{ ⟨ ∀ i : 0 ≤ i < x : Inv i ⟩ }"
          ]
          @?= ["True ⇒ ⟨ ∀ $1 : Int : 0 ≤ $1 < x + 1 : x + 1 < $1 ⟩"]
    ]

-- | Cases where a capture would be wrong: a local binder meeting a
--   definition's global, a parameter sharing a global's name, or
--   substitution keys incorrectly capturing free variables in replacement
--   expressions, whether within one table or during composition. These guard the
--   capture-avoidance that a fix for 'substitutionReachesDefinitionBodies'
--   must keep.
bindersStillAvoidCapture :: ([Text] -> [Text]) -> TestTree
bindersStillAvoidCapture solve =
  testGroup
    "binders other than a substitution still avoid capture"
    [ -- Issue 182.  The case binder r = C is local; Inv's r is the global.
      --
      -- case C of r → Inv B
      --   = case C of r' → r < B
      --   = r < B
      testCase "issue 182: a case binder does not capture a definition's global" $
        solve
          [ "var r : Int",
            "con B, C : Int",
            "{:",
            "  Inv t = r < t",
            ":}",
            "{ True }",
            "skip",
            "{ case C of r -> Inv B }"
          ]
          @?= ["True ⇒ r < B"],
      -- ⟨∀ r : True : Inv r⟩
      --   = ⟨∀ r' : True : r < r'⟩      not  ⟨∀ r : True : r < r⟩
      testCase "a quantifier binder does not capture a definition's global" $
        solve
          [ "var r : Int",
            "{:",
            "  Inv t = r < t",
            ":}",
            "{ True }",
            "skip",
            "{ ⟨ ∀ r : True : Inv r ⟩ }"
          ]
          @?= ["True ⇒ ⟨ ∀ $1 : Int : True : r < $1 ⟩"],
      -- Issue 180.  The bound inv hides the definition, so nothing is
      -- clickable.
      --
      -- ⟨∀ inv : True : inv B⟩      stays as it is
      testCase "issue 180: a quantifier binder hides a same-named definition" $
        solve
          [ "con B : Int",
            "{:",
            "  inv x = x > 42",
            ":}",
            "{ True }",
            "skip",
            "{ ⟨ ∀ inv : True : inv B ⟩ }"
          ]
          @?= ["True ⇒ ⟨ ∀ $1 : Int → Bool : True : $1 B ⟩"],
      -- No definition here.  The outer table's value x is the variable before
      -- y := x, while the inner table's key x is the variable x := 1 updates:
      -- two different states of x.  Composing the tables must therefore still
      -- rename the inner key rather than let it capture the incoming x.
      --
      -- ((x < y)[x \ 1])[y \ x]
      --   = (1 < y)[y \ x]
      --   = 1 < x              not  (x < x)[x \ 1] = 1 < 1
      testCase "composing two substitutions keeps their x apart" $
        solve
          [ "var x, y : Int",
            "{ True }",
            "y := x",
            "; x := 1",
            "{ x < y }"
          ]
          @?= ["True ⇒ 1 < x"],
      -- Inv's x is its parameter, not the global, so only the argument changes.
      --
      -- (Inv x)[x \ 3]
      --   = Inv 3
      --   = 3 < B
      testCase "a parameter that shares the global's name is not substituted" $
        solve
          [ "var x : Int",
            "con B : Int",
            "{:",
            "  Inv x = x < B",
            ":}",
            "{ True }",
            "x := 3",
            "{ Inv x }"
          ]
          @?= ["True ⇒ 3 < B"],
      -- P's body has no global, so only the argument changes.
      --
      -- (P x)[x \ x + 1]
      --   = P (x + 1)
      --   = B < x + 1
      testCase "a definition without globals only sees its argument change" $
        solve
          [ "var x : Int",
            "con B : Int",
            "{:",
            "  P t = B < t",
            ":}",
            "{ True }",
            "x := x + 1",
            "{ P x }"
          ]
          @?= ["True ⇒ B < x + 1"],
      -- (r = 3)[r \ add 1 2]
      --   = add 1 2 = 3
      --   = 1 + 2 = 3
      testCase "a definition call on the right of an assignment" $
        solve
          [ "var r : Int",
            "{:",
            "  add x y = x + y",
            ":}",
            "{ True }",
            "r := add 1 2",
            "{ r = 3 }"
          ]
          @?= ["True ⇒ 1 + 2 = 3"],
      -- K reads x when y := K runs, before the later x := 1.
      --
      -- ((y = 0)[x \ 1])[y \ K]
      --   = (y = 0)[y \ K]
      --   = K = 0
      --   = x = 0
      testCase "a definition on an assignment's right reads the global before a later assignment" $
        solve
          [ "var x, y : Int",
            "{:",
            "  K = x",
            ":}",
            "{ True }",
            "y := K",
            "; x := 1",
            "{ y = 0 }"
          ]
          @?= ["True ⇒ x = 0"],
      -- K reads x when y := K runs, after the earlier x := 1.
      --
      -- ((y = 0)[y \ K])[x \ 1]
      --   = (K = 0)[x \ 1]
      --   = (x = 0)[x \ 1]
      --   = 1 = 0
      testCase "a definition on an assignment's right sees an earlier assignment" $
        solve
          [ "var x, y : Int",
            "{:",
            "  K = x",
            ":}",
            "{ True }",
            "x := 1",
            "; y := K",
            "{ y = 0 }"
          ]
          @?= ["True ⇒ 1 = 0"],
      -- K's x is global; the postcondition's x is local to its quantifier.
      --
      -- ⟨∀ x : True : y = x⟩[y \ K]
      --   = ⟨∀ x' : True : K = x'⟩
      --   = ⟨∀ x' : True : x = x'⟩
      testCase "an assignment's definition value stays free of the postcondition's binder" $
        solve
          [ "var x, y : Int",
            "{:",
            "  K = x",
            ":}",
            "{ True }",
            "y := K",
            "{ ⟨ ∀ x : True : y = x ⟩ }"
          ]
          @?= ["True ⇒ ⟨ ∀ $1 : Int : True : x = $1 ⟩"],
      -- Passing K through F's parameter must keep K's global x free.
      --
      -- F K
      --   = ⟨∀ x' : 0 ≤ x' : K = x'⟩
      --   = ⟨∀ x' : 0 ≤ x' : x = x'⟩
      testCase "a definition passed as an argument keeps its global free" $
        solve
          [ "var x : Int",
            "{:",
            "  K = x",
            "  F t = ⟨ ∀ x : 0 ≤ x : t = x ⟩",
            ":}",
            "{ True }",
            "skip",
            "{ F K }"
          ]
          @?= ["True ⇒ ⟨ ∀ $1 : Int : 0 ≤ $1 : x = $1 ⟩"],
      -- f = helper, helper = x. The quantifier's local helper must not hide
      -- the definition reference introduced by unfolding f.
      --
      -- ⟨∀ helper : True : y = helper⟩[y \ f]
      --   = ⟨∀ helper' : True : f = helper'⟩
      --   = ⟨∀ helper' : True : helper = helper'⟩
      --   = ⟨∀ helper' : True : x = helper'⟩
      testCase "an alias resolves its target in the definition's declaration scope" $
        solve
          [ "var x, y : Int",
            "{:",
            "  f = helper",
            "  helper = x",
            ":}",
            "{ True }",
            "y := f",
            "{ ⟨ ∀ helper : True : y = helper ⟩ }"
          ]
          @?= ["True ⇒ ⟨ ∀ $1 : Int : True : x = $1 ⟩"],
      -- Both right-hand sides read the state before the simultaneous
      -- assignment, so K's x is not the 1 being assigned to x.
      --
      -- (y = 0)[x, y \ 1, K]
      --   = K = 0
      --   = x = 0
      testCase "a definition on a simultaneous assignment's right reads the old global" $
        solve
          [ "var x, y : Int",
            "{:",
            "  K = x",
            ":}",
            "{ True }",
            "x, y := 1, K",
            "{ y = 0 }"
          ]
          @?= ["True ⇒ x = 0"],
      -- (f a b = 0)[a \ 0]
      --   = f 0 b = 0
      --   = b = 0          by the first clause, f 0 y = y
      testCase "a pattern-matching definition picks the clause the substitution enables" $
        solve
          [ "{:",
            "  f : Int -> Int -> Int",
            "  f 0 y = y",
            "  f x y = x + y",
            ":}",
            "var a, b : Int",
            "{ True }",
            "a := 0",
            "{ f a b = 0 }"
          ]
          @?= ["True ⇒ b = 0"]
    ]

-- | Every result reachable by clicking something in @expression@, keeping only
--   those that still contain a lambda.
lambdasLeft ::
  (DefnEnv -> T.Expr -> RedexPath -> Maybe T.Expr) ->
  (DefnEnv -> T.Expr -> [RedexPath]) ->
  (DefnEnv, T.Expr) ->
  [T.Expr]
lambdasLeft reduce paths (defnEnv, expression) =
  [ result
  | path <- paths defnEnv expression,
    Just result <- [reduce defnEnv expression path],
    hasLam result
  ]

hasLam :: T.Expr -> Bool
hasLam T.Lam {} = True
hasLam expression = any hasLam (pathChildren expression)

stateDependentPaths ::
  (DefnEnv -> T.Expr -> RedexPath -> State Int (Maybe T.Expr)) ->
  (DefnEnv, T.Expr) ->
  [RedexPath]
stateDependentPaths reduce (defnEnv, expression) =
  [ path
  | path <- allPaths expression,
    varies
      [ isJust (evalState (reduce defnEnv expression path) initialState)
      | initialState <- [0, 1, 7, 1000 :: Int]
      ]
  ]

allPaths :: T.Expr -> [RedexPath]
allPaths expression =
  []
    : concat
      [ map (index :) (allPaths child)
      | (index, child) <- zip [0 ..] (pathChildren expression)
      ]

varies :: (Eq a) => [a] -> Bool
varies [] = False
varies (firstValue : rest) = any (/= firstValue) rest

reductionCases :: [(DefnEnv, T.Expr)]
reductionCases =
  [ (binaryDefnEnv, fullCall),
    (binaryDefnEnv, partialCall),
    (binaryDefnEnv, argumentCall),
    (binaryDefnEnv, shadowed),
    (unaryDefnEnv, unaryCall),
    (aliasDefnEnv, var f pairFunctionType),
    (aliasDefnEnv, T.App (var f pairFunctionType) (var a intType) Nothing),
    ([(f, var i intType)], captureCandidate),
    ([(f, var x intType)], pendingSubst)
  ]

f, g, h, x, y, a, b, i :: Name
f = Name "f" Nothing
g = Name "g" Nothing
h = Name "h" Nothing
x = Name "x" Nothing
y = Name "y" Nothing
a = Name "a" Nothing
b = Name "b" Nothing
i = Name "i" Nothing

intType, boolType, unaryType :: A.Type
intType = A.TBase A.TInt Nothing
boolType = A.TBase A.TBool Nothing
unaryType = A.mkArrowType intType intType

pairFunctionType, additionType, comparisonType :: A.Type
pairFunctionType = A.mkArrowType intType (A.mkArrowType intType (A.TTuple [intType, intType]))
additionType = A.mkArrowType intType (A.mkArrowType intType intType)
comparisonType = A.mkArrowType intType (A.mkArrowType intType boolType)

var :: Name -> A.Type -> T.Expr
var name t = T.Var name t Nothing

identity :: T.Expr
identity = T.Lam x intType (var x intType) Nothing

binaryDefinition :: T.Expr
binaryDefinition =
  T.Lam x intType (T.Lam y intType (T.Tuple [var x intType, var y intType]) Nothing) Nothing

unaryDefnEnv, binaryDefnEnv, aliasDefnEnv :: DefnEnv
unaryDefnEnv = [(h, identity)]
binaryDefnEnv = [(g, binaryDefinition)]
aliasDefnEnv = [(f, var g pairFunctionType), (g, binaryDefinition)]

unaryCall, partialCall, fullCall, argumentCall :: T.Expr
unaryCall = T.App (var h unaryType) (var a intType) Nothing
partialCall = T.App (var g pairFunctionType) (var a intType) Nothing
fullCall = T.App partialCall (var b intType) Nothing
argumentCall =
  T.App
    (var h (A.mkArrowType pairFunctionType intType))
    (var g pairFunctionType)
    Nothing

truth :: T.Expr
truth = T.Lit (A.Bol True) boolType Nothing

shadowed :: T.Expr
shadowed = T.Quant truth [(g, pairFunctionType)] truth (var g pairFunctionType) Nothing

captureCandidate :: T.Expr
captureCandidate =
  T.Quant
    truth
    [(i, intType)]
    truth
    (T.Tuple [var i intType, var f intType])
    Nothing

-- | A pending substitution whose table binds @x@ over the subject.
pendingSubst :: T.Expr
pendingSubst =
  T.Subst (T.Tuple [var x intType, var f intType]) [(x, var a intType)]

-- | One expression per 'T.Expr' constructor, in constructor order; add one
--   when you add a constructor.
everyConstructor :: [T.Expr]
everyConstructor =
  [ T.Lit (A.Num 1) intType Nothing,
    var a intType,
    T.Const b intType Nothing,
    T.Op (ArithOp (Add Nothing)) additionType,
    T.Chain (T.More (T.Pure (var a intType)) (ChainOp (LTE Nothing)) comparisonType (var b intType)),
    partialCall,
    identity,
    T.Tuple [var a intType, var b intType],
    T.OutT 1 (T.Tuple [var a intType, var b intType]),
    T.Quant
      (T.Op (ArithOp (Add Nothing)) additionType)
      [(i, intType)]
      truth
      (var i intType)
      Nothing,
    T.ArrIdx (var a intType) (var b intType) Nothing,
    T.ArrUpd (var a intType) (var b intType) (var i intType) Nothing,
    T.Case truth [T.CaseClause (A.PattBinder x) (var a intType)] Nothing,
    pendingSubst,
    T.EHole (T.Hole "hole" 0 intType holeRange mempty)
  ]

holeRange :: Range
holeRange = mkRange (mkPos 1 1) (mkPos 1 2)
