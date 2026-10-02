{-# LANGUAGE OverloadedStrings #-}

module Test.Reduce2 (tests) where

import Control.Monad.State (evalState)
import Data.Maybe (isJust)
import GCL.Common (initFreshState)
import GCL.Range (Range, mkPos, mkRange)
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (ArithOp (..), Name (..), Op (..))
import Syntax.Typed.Reduce2
import qualified Syntax.Typed.Types as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Reduce2"
    [ testCase "an alias unfolds exactly once" $
        run aliasEnv (var f binaryType) [] @?= Just (var g binaryType),
      testCase "a self-reference still counts as an unfold step" $
        run [(f, var f binaryType)] (var f binaryType) []
          @?= Just (var f binaryType),
      testCase "a fully applied definition repairs every leading lambda" $
        run binaryEnv fullCall [0, 0] @?= Just (T.Tuple [var a intType, var b intType]),
      testCase "a one-parameter definition consumes its single application" $
        run unaryEnv unaryCall [0] @?= Just (var a intType),
      testCase "a partial application is not clickable" $
        run binaryEnv partialCall [0] @?= Nothing,
      testCase "an argument cannot borrow its parent application" $
        run binaryEnv argumentCall [1] @?= Nothing,
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
        reduciblePaths binaryEnv fullCall @?= [[0, 0]],
      testCase "clickability is independent of initial fresh-name state" $
        concatMap stateDependentPaths reductionCases @?= [],
      testCase "a lexical binder hides a same-named definition" $
        run binaryEnv shadowed [2] @?= Nothing,
      testCase "unfolding through an ancestor binder avoids capture" $
        case run [(f, var i intType)] captureCandidate [2, 1] of
          Just (T.Quant _ [(binder, _)] _ (T.Tuple [bound, inserted]) _) -> do
            binder /= i @? "the quantifier binder must be renamed"
            bound @?= var binder intType
            inserted @?= var i intType
          result -> assertFailure ("unexpected result: " <> show result),
      -- A substitution table binds its domain over the subject, so an
      -- occurrence there is already spoken for: it is on its way to being
      -- replaced by the table, not a reference to a global definition.
      testCase "a substitution table hides a same-named definition" $
        run [(x, var a intType)] pendingSubst [0, 0] @?= Nothing,
      -- The same scope, seen from the other side: a definition unfolded under
      -- the table must not be captured by it, so the table's binder is the one
      -- that moves.
      testCase "unfolding under a substitution table avoids capture" $
        case run [(f, var x intType)] pendingSubst [0, 1] of
          Just (T.Subst (T.Tuple [bound, inserted]) [(binder, _)]) -> do
            binder /= x @? "the table binder must be renamed"
            bound @?= var binder intType
            inserted @?= var x intType
          result -> assertFailure ("unexpected result: " <> show result),
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
        run binaryEnv fullCall [7] @?= Nothing
    ]

run :: Env -> T.Expr -> RedexPath -> Maybe T.Expr
run env expression path =
  evalState (tryReduceAt env expression path) initFreshState

-- | Every result reachable by clicking something in @expression@, keeping only
--   those that still contain a lambda.
lambdasLeft :: (Env, T.Expr) -> [T.Expr]
lambdasLeft (env, expression) =
  [ result
  | path <- reduciblePaths env expression,
    Just result <- [run env expression path],
    hasLam result
  ]

hasLam :: T.Expr -> Bool
hasLam T.Lam {} = True
hasLam expression = any hasLam (pathChildren expression)

stateDependentPaths :: (Env, T.Expr) -> [RedexPath]
stateDependentPaths (env, expression) =
  [ path
  | path <- allPaths expression,
    varies
      [ isJust (evalState (tryReduceAt env expression path) initialState)
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

reductionCases :: [(Env, T.Expr)]
reductionCases =
  [ (binaryEnv, fullCall),
    (binaryEnv, partialCall),
    (binaryEnv, argumentCall),
    (binaryEnv, shadowed),
    (unaryEnv, unaryCall),
    (aliasEnv, var f binaryType),
    (aliasEnv, T.App (var f binaryType) (var a intType) Nothing),
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

unaryEnv, binaryEnv, aliasEnv :: Env
unaryEnv = [(h, identity)]
binaryEnv = [(g, binaryDefinition)]
aliasEnv = [(f, var g binaryType), (g, binaryDefinition)]

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
