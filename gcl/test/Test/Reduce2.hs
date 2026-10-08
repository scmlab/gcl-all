{-# LANGUAGE OverloadedStrings #-}

module Test.Reduce2 (tests) where

import Control.Monad.State (State, evalState, state)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Common (initFreshState)
import GCL.Predicate (PO (..))
import GCL.Range (Range, mkPos, mkRange)
import Pretty (toText)
import Server.FileState (FileState (..))
import Server.Load (loadAndDig)
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (ArithOp (..), Name (..), Op (..), nameToText)
import Syntax.Typed.Reduce2
import Syntax.Typed.Subst2 (renameFree)
import qualified Syntax.Typed.Types as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Reduce2"
    [ testCase "an alias unfolds exactly once" $
        run aliasDefnEnv (var f binaryType) [] @?= Just (var g binaryType),
      testCase "a self-reference still counts as an unfold step" $
        run [(f, var f binaryType)] (var f binaryType) []
          @?= Just (var f binaryType),
      testCase "a fully applied definition repairs every leading lambda" $
        run binaryDefnEnv fullCall [0, 0] @?= Just (T.Tuple [var a intType, var b intType]),
      testCase "a one-parameter definition consumes its single application" $
        run unaryDefnEnv unaryCall [0] @?= Just (var a intType),
      testCase "a partial application is not clickable" $
        run binaryDefnEnv partialCall [0] @?= Nothing,
      testCase "an argument cannot borrow its parent application" $
        run binaryDefnEnv argumentCall [1] @?= Nothing,
      testCase "tuple projection reduces locally" $
        run [] (T.OutT 1 (T.Tuple [var a intType, var b intType])) []
          @?= Just (var b intType),
      testCase "an explicit substitution reduces locally" $
        run [] (T.Subst (T.Tuple [var x intType, var y intType]) [(x, var a intType)]) []
          @?= Just (T.Tuple [var a intType, var y intType]),
      testCase "case skips a refuted clause" $
        run
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
      testCase "an undecidable case clause blocks later clauses" $
        run
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
      testCase "redex discovery and execution use the same judgement" $
        reduciblePaths binaryDefnEnv fullCall @?= [[0, 0]],
      testCase "clickability is independent of initial fresh-name state" $
        concatMap stateDependentPaths reductionCases @?= [],
      testCase "a lexical binder hides a same-named definition" $
        run binaryDefnEnv shadowed [2] @?= Nothing,
      testCase "unfolding through an ancestor binder avoids capture" $
        case run [(f, var i intType)] captureCandidate [2, 1] of
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
        run [(x, var a intType)] pendingSubst [0, 0] @?= Nothing,
      -- f = x, where x is the global the table replaces.
      --
      -- (x, f)[x \ a]
      --   = (x, x)[x \ a]      the table captures the unfolded x
      --   = (a, a)
      --
      -- A table is not a lexical binder: it names the program variable the
      -- assignment updates, which is the same x a definition body refers to.
      testCase "unfolding under a substitution table lets the table capture" $
        run [(f, var x intType)] pendingSubst [0, 1]
          @?= Just (T.Subst (T.Tuple [var x intType, var x intType]) [(x, var a intType)]),
      -- Reduce2 inspects only the root of its moving focus for a lambda, which
      -- is sound because a definition body is itself lambda-free.  This pins
      -- that premise rather than any one code path: it fails as soon as a
      -- lambda survives into a result the UI would be handed.
      testCase "every reducible path yields a lambda-free result" $
        concatMap lambdasLeft reductionCases @?= [],
      -- 'pathChildren' and 'withPathChildren' are two hand-written lists that
      -- have to agree on every constructor, and where they drift
      -- 'withPathChildren' throws at run time rather than failing to compile.
      -- The children in each sample are distinct, so an ordering drift also
      -- changes the rebuilt expression.
      testCase "withPathChildren rebuilds every constructor unchanged" $
        filter (\e -> withPathChildren e (pathChildren e) /= e) everyConstructor @?= [],
      testCase "invalid paths are rejected" $
        run binaryDefnEnv fullCall [7] @?= Nothing,
      substitutionReachesDefinitionBodies,
      bindersStillAvoidCapture
    ]

-- | Each case is a whole GCL program with one proof obligation.  'answers'
--   clicks every redex in every order, so a single expected answer also
--   asserts that the click order does not matter.
--
--   An assignment @v := e@ leaves @Q[v \ e]@ in the obligation.  A definition
--   body's free names are the program's globals, so the pending substitution
--   must reach them once the definition is unfolded.
substitutionReachesDefinitionBodies :: TestTree
substitutionReachesDefinitionBodies =
  testGroup
    "a substitution reaches the globals inside a definition body"
    [ -- (Inv B)[d \ 5]
      --   = (d + x < B)[d \ 5]
      --   = 5 + x < B
      testCase "a single assignment" $
        answers
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
        answers
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
        answers
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
        answers
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
        answers
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
        answers
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
        answers
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
      -- The case binder r is local; Inv's r is the global, which the
      -- assignment replaces.
      --
      -- (case C of r → Inv B)[r \ 7]
      --   = (case C of r' → r < B)[r \ 7]
      --   = case C of r' → 7 < B
      --   = 7 < B
      testCase "a case binder between the assignment and the definition" $
        answers
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
        answers
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
        answers
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
        answers
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
bindersStillAvoidCapture :: TestTree
bindersStillAvoidCapture =
  testGroup
    "binders other than a substitution still avoid capture"
    [ -- Issue 182.  The case binder r = C is local; Inv's r is the global.
      --
      -- case C of r → Inv B
      --   = case C of r' → r < B
      --   = r < B
      testCase "issue 182: a case binder does not capture a definition's global" $
        answers
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
        answers
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
        answers
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
        answers
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
        answers
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
        answers
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
        answers
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
        answers
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
      -- Both right-hand sides read the state before the simultaneous
      -- assignment, so K's x is not the 1 being assigned to x.
      --
      -- (y = 0)[x, y \ 1, K]
      --   = K = 0
      --   = x = 0
      testCase "a definition on a simultaneous assignment's right reads the old global" $
        answers
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
        answers
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

-- | Load a program with exactly one proof obligation, click every redex in
--   every order, requiring each marked path to reduce successfully, and
--   render each predicate in which nothing is clickable any more.  See
--   'display' for how the answers are normalised.
answers :: [Text] -> [Text]
answers sourceLines =
  case loadAndDig "<test>" (Text.unlines sourceLines) of
    Right (_, Right fileState)
      | [po] <- fsProofObligations fileState ->
          normalForms (fsDefinitions fileState) (poReducedPred po)
    _ -> error "the test program must load with exactly one proof obligation"

normalForms :: DefnEnv -> T.Expr -> [Text]
normalForms defnEnv start = Set.toList (go Set.empty Set.empty [start])
  where
    go _ found [] = found
    go seen found (expression : pending)
      | key `Set.member` seen = go seen found pending
      | Set.size seen > 1000 = error "too many reachable states; a definition may be recursive"
      | null successors = go seen' (Set.insert (display expression) found) pending
      | otherwise = go seen' found (successors <> pending)
      where
        key = toText expression
        seen' = Set.insert key seen
        successors = map reducePath (reduciblePaths defnEnv expression)
        reducePath path =
          case run defnEnv expression path of
            Just result -> result
            Nothing ->
              error
                ( "a marked redex failed to reduce at path "
                    <> show path
                    <> " in predicate: "
                    <> Text.unpack key
                )

-- | Render with every quantifier binder renamed to @$1@, @$2@, … in order of
--   appearance and runs of spaces collapsed, so that answers differing only
--   in which fresh name a click order drew compare equal.
display :: T.Expr -> Text
display expression =
  Text.unwords (Text.words (toText (evalState (canonicalBinders expression) (1 :: Int))))

canonicalBinders :: T.Expr -> State Int T.Expr
canonicalBinders (T.Quant operator binders range body l) = do
  canonical <- mapM (const nextBinder) binders
  let renaming = zip (map (nameToText . fst) binders) canonical
  T.Quant
    <$> canonicalBinders operator
    <*> pure [(Name text r, t) | ((Name _ r, t), text) <- zip binders canonical]
    <*> canonicalBinders (renameFree renaming range)
    <*> canonicalBinders (renameFree renaming body)
    <*> pure l
  where
    nextBinder = state (\k -> ("$" <> Text.pack (show k), k + 1))
canonicalBinders expression =
  withPathChildren expression <$> mapM canonicalBinders (pathChildren expression)

run :: DefnEnv -> T.Expr -> RedexPath -> Maybe T.Expr
run defnEnv expression path =
  evalState (tryReduceAt defnEnv expression path) initFreshState

reduciblePaths :: DefnEnv -> T.Expr -> [RedexPath]
reduciblePaths defnEnv expression = collect [] (redexRT defnEnv expression)
  where
    collect path (Node reducible children) =
      [path | reducible]
        <> concat
          [ collect (path <> [index]) child
          | (index, child) <- zip [0 ..] children
          ]

-- | Every result reachable by clicking something in @expression@, keeping only
--   those that still contain a lambda.
lambdasLeft :: (DefnEnv, T.Expr) -> [T.Expr]
lambdasLeft (defnEnv, expression) =
  [ result
  | path <- reduciblePaths defnEnv expression,
    Just result <- [run defnEnv expression path],
    hasLam result
  ]

hasLam :: T.Expr -> Bool
hasLam T.Lam {} = True
hasLam expression = any hasLam (pathChildren expression)

stateDependentPaths :: (DefnEnv, T.Expr) -> [RedexPath]
stateDependentPaths (defnEnv, expression) =
  [ path
  | path <- allPaths expression,
    varies
      [ isJust (evalState (tryReduceAt defnEnv expression path) initialState)
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
    (aliasDefnEnv, var f binaryType),
    (aliasDefnEnv, T.App (var f binaryType) (var a intType) Nothing),
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

intType, boolType, unaryType, binaryType :: A.Type
intType = A.TBase A.TInt Nothing
boolType = A.TBase A.TBool Nothing
unaryType = A.mkArrowType intType intType
binaryType = A.mkArrowType intType (A.mkArrowType intType intType)

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
aliasDefnEnv = [(f, var g binaryType), (g, binaryDefinition)]

unaryCall, partialCall, fullCall, argumentCall :: T.Expr
unaryCall = T.App (var h unaryType) (var a intType) Nothing
partialCall = T.App (var g binaryType) (var a intType) Nothing
fullCall = T.App partialCall (var b intType) Nothing
argumentCall =
  T.App
    (var h (A.mkArrowType binaryType intType))
    (var g binaryType)
    Nothing

truth :: T.Expr
truth = T.Lit (A.Bol True) boolType Nothing

shadowed :: T.Expr
shadowed = T.Quant truth [(g, binaryType)] truth (var g binaryType) Nothing

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
    T.Op (ArithOp (Add Nothing)) binaryType,
    T.Chain (T.More (T.Pure (var a intType)) (ArithOp (Add Nothing)) binaryType (var b intType)),
    partialCall,
    identity,
    T.Tuple [var a intType, var b intType],
    T.OutT 1 (T.Tuple [var a intType, var b intType]),
    T.Quant
      (T.Op (ArithOp (Add Nothing)) binaryType)
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
