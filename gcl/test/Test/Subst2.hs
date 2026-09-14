{-# LANGUAGE OverloadedStrings #-}

module Test.Subst2 (tests) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Control.Monad.State (evalState)
import qualified Data.Map as Map
import GCL.Range (mkPos, mkRange)
import GCL.WP (runWP)
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Concrete.Instances.ToAbstract ()
import Syntax.Typed.Subst2 (substExpr)
import qualified Syntax.Typed.Types as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Subst2"
    [ testCase "case-pattern binders shadow substitutions" $ do
        let clause =
              caseOf
                scrutinee
                (A.PattBinder x)
                (T.Tuple [var x, var y])
            expected = caseOf scrutinee (A.PattBinder x) (T.Tuple [var x, two])

        run [("x", one), ("y", two)] clause @?= expected,
      testCase "case scrutinee is substituted" $ do
        let clause = caseOf (var x) (A.PattBinder x) (var x)

        run [("x", one)] clause @?= caseOf one (A.PattBinder x) (var x),
      testCase "case-pattern substitution avoids capture" $
        case run [("y", var x)] (caseOf scrutinee (A.PattBinder x) (T.Tuple [var x, var y])) of
          T.Case _ [T.CaseClause (A.PattBinder binder) (T.Tuple [bound, inserted])] _ -> do
            nameToText binder /= "x" @? "the pattern binder must be renamed"
            bound @?= var binder
            inserted @?= var x
          result -> assertFailure ("unexpected result: " <> show result),
      testCase "capture avoidance reaches composite patterns" $
        case run
          [("y", var x)]
          ( caseOf
              scrutinee
              (A.PattConstructor just [A.PattTuple [A.PattBinder x]])
              (T.Tuple [var x, var y])
          ) of
          T.Case
            _
            [ T.CaseClause
                (A.PattConstructor constructor [A.PattTuple [A.PattBinder binder]])
                (T.Tuple [bound, inserted])
              ]
            _ -> do
              constructor @?= just
              nameToText binder /= "x" @? "the nested pattern binder must be renamed"
              bound @?= var binder
              inserted @?= var x
          result -> assertFailure ("unexpected result: " <> show result),
      testCase "a colliding WP fresh candidate is rejected" $ do
        let clause = caseOf scrutinee (A.PattBinder x) (T.Tuple [var x, var y])
            check scopes =
              case runWP (substExpr [("y", var x)] clause) (Map.empty, scopes) 0 of
                Right (T.Case _ [T.CaseClause (A.PattBinder binder) (T.Tuple [bound, inserted])] _, _, _) -> do
                  nameToText binder /= "x" @? "the WP candidate must be rejected"
                  bound @?= var binder
                  inserted @?= var x
                Right (result, _, _) -> assertFailure ("unexpected result: " <> show result)
                Left err -> assertFailure ("unexpected WP failure: " <> show err)

        mapM_ check [[], [["a", "b", "c"]]],
      testCase "an inner binder of the same name is left alone" $ do
        -- Renaming the clause binder must not disturb a nested @\\x -> x@: it
        -- shadows, so nothing inside it refers to the clause binder.
        let nested = T.Lam x intType (var x) Nothing

        case run [("y", var x)] (caseOf scrutinee (A.PattBinder x) (T.Tuple [nested, var y])) of
          T.Case _ [T.CaseClause (A.PattBinder binder) (T.Tuple [lambda, inserted])] _ -> do
            nameToText binder /= "x" @? "the clause binder must be renamed"
            lambda @?= nested
            inserted @?= var x
          result -> assertFailure ("unexpected result: " <> show result),
      testCase "an inner binder that would capture is renamed" $ do
        -- Here the nested lambda does not shadow the inserted name, so entering
        -- it with @x@ live in the range forces the lambda's own binder to move.
        let nested = T.Lam x intType (var y) Nothing

        case run [("y", var x)] (T.Tuple [nested, var y]) of
          T.Tuple [T.Lam binder _ body _, inserted] -> do
            nameToText binder /= "x" @? "the lambda binder must be renamed"
            body @?= var x
            inserted @?= var x
          result -> assertFailure ("unexpected result: " <> show result),
      testCase "a renamed occurrence keeps its own range" $ do
        let binderName = Name "x" (Just (mkRange (mkPos 1 1) (mkPos 1 2)))
            occurrence = Name "x" (Just (mkRange (mkPos 2 5) (mkPos 2 6)))
            clause =
              caseOf
                scrutinee
                (A.PattBinder binderName)
                (T.Tuple [T.Var occurrence intType Nothing, var y])

        case run [("y", var x)] clause of
          T.Case _ [T.CaseClause (A.PattBinder binder) (T.Tuple [T.Var bound _ _, _])] _ -> do
            nameToText binder /= "x" @? "the pattern binder must be renamed"
            rangeOfName binder @?= rangeOfName binderName
            rangeOfName bound @?= rangeOfName occurrence
          result -> assertFailure ("unexpected result: " <> show result),
      -- issue 02: a shadowed entry must not take the rest of the
      -- substitution down with it.
      testCase "a lambda binder shadows only its own entry" $ do
        let lambda = T.Lam x intType (T.Tuple [var x, var y]) Nothing
            expected = T.Lam x intType (T.Tuple [var x, two]) Nothing

        run [("x", one), ("y", two)] lambda @?= expected,
      testCase "lambda substitution avoids capture" $
        case run [("y", var x)] (T.Lam x intType (T.Tuple [var x, var y]) Nothing) of
          T.Lam binder _ (T.Tuple [bound, inserted]) _ -> do
            nameToText binder /= "x" @? "the lambda binder must be renamed"
            bound @?= var binder
            inserted @?= var x
          result -> assertFailure ("unexpected result: " <> show result),
      -- issue 03: Fresh WP only avoids names in its reader scopes, so every
      -- candidate has to be checked against the expression itself.
      testCase "a colliding WP fresh candidate is rejected for lambdas" $ do
        let lambda = T.Lam x intType (T.Tuple [var x, var y]) Nothing
            check scopes =
              case runIn scopes [("y", var x)] lambda of
                Right (T.Lam binder _ (T.Tuple [bound, inserted]) _) -> do
                  nameToText binder /= "x" @? "the WP candidate must be rejected"
                  bound @?= var binder
                  inserted @?= var x
                Right result -> assertFailure ("unexpected result: " <> show result)
                Left err -> assertFailure ("unexpected WP failure: " <> err)

        mapM_ check [[], [["a", "b", "c"]]],
      testCase "a WP candidate colliding with the range is rejected" $ do
        -- With "x" in scope Fresh WP proposes "x0", which is not the binder's
        -- own name but does occur free in the range. It must be rejected too.
        let lambda = T.Lam x intType (var y) Nothing
            range = T.Tuple [var x, var x0]

        case runIn [["x"]] [("y", range)] lambda of
          Right (T.Lam binder _ body _) -> do
            nameToText binder /= "x" @? "the binder must move"
            nameToText binder /= "x0" @? "x0 is free in the range and must be rejected"
            body @?= range
          Right result -> assertFailure ("unexpected result: " <> show result)
          Left err -> assertFailure ("unexpected WP failure: " <> err),
      testCase "renaming a binder cascades into an inner binder of the target name" $ do
        -- Under Fresh WP the clause binder y is forced to y'. The inner lambda
        -- is already spelled y', so it has to move as well.
        let clause =
              caseOf
                scrutinee
                (A.PattBinder y)
                (T.Lam (Name "y'" Nothing) intType (T.Tuple [var y, var x]) Nothing)

        case runIn [] [("x", var y)] clause of
          Right (T.Case _ [T.CaseClause (A.PattBinder outer) (T.Lam inner _ (T.Tuple [bound, inserted]) _)] _) -> do
            nameToText outer /= "y" @? "the clause binder must be renamed"
            nameToText inner /= nameToText outer
              @? "the inner binder must not shadow the renamed clause binder"
            bound @?= var outer
            inserted @?= var y
          Right result -> assertFailure ("unexpected result: " <> show result)
          Left err -> assertFailure ("unexpected WP failure: " <> err),
      testCase "quantifier binders shadow substitutions" $ do
        let quantified = quantOf i (T.Tuple [var i, var y]) (var i)
            expected = quantOf i (T.Tuple [var i, two]) (var i)

        run [("i", one), ("y", two)] quantified @?= expected,
      -- The first i is the operator; the second is the quantifier binder:
      --   ⟨ i i : i : i ⟩[i := 1] → ⟨ 1 i : i : i ⟩
      testCase "quantifier binders do not scope over the operator" $ do
        let occurrence = var i
            quantified = T.Quant occurrence [(i, intType)] occurrence occurrence Nothing
            expected = T.Quant one [(i, intType)] occurrence occurrence Nothing

        run [("i", one)] quantified @?= expected,
      -- The operator is outside the binder's scope, so substituting i there does
      -- not require renaming the binder:
      --   ⟨ op i : i : i ⟩[op := i] → ⟨ i i : i : i ⟩
      testCase "operator substitution does not rename quantifier binders" $ do
        let occurrence = var i
            quantified = quantOf i occurrence occurrence
            expected = T.Quant occurrence [(i, intType)] occurrence occurrence Nothing

        run [("op", occurrence)] quantified @?= expected,
      testCase "quantifier substitution avoids capture" $
        case run [("y", var i)] (quantOf i (T.Tuple [var i, var y]) (var i)) of
          T.Quant _ [(binder, _)] (T.Tuple [bound, inserted]) body _ -> do
            nameToText binder /= "i" @? "the quantifier binder must be renamed"
            bound @?= var binder
            inserted @?= var i
            body @?= var binder
          result -> assertFailure ("unexpected result: " <> show result),
      -- issue 05: alpha-renaming must not hand every occurrence the binder's
      -- range. Pretty-printed output does not show ranges, so assert on the AST.
      testCase "a renamed lambda keeps binder and occurrence ranges apart" $ do
        let binderName = Name "x" (Just (mkRange (mkPos 1 1) (mkPos 1 2)))
            occurrence = Name "x" (Just (mkRange (mkPos 2 5) (mkPos 2 6)))
            lambda =
              T.Lam
                binderName
                intType
                (T.Tuple [T.Var occurrence intType Nothing, var y])
                Nothing

        case run [("y", var x)] lambda of
          T.Lam binder _ (T.Tuple [T.Var bound _ _, _]) _ -> do
            rangeOfName binder @?= rangeOfName binderName
            rangeOfName bound @?= rangeOfName occurrence
          result -> assertFailure ("unexpected result: " <> show result),
      testCase "a renamed quantifier keeps binder and occurrence ranges apart" $ do
        let binderName = Name "i" (Just (mkRange (mkPos 1 1) (mkPos 1 2)))
            occurrence = Name "i" (Just (mkRange (mkPos 2 5) (mkPos 2 6)))
            quantified =
              T.Quant
                quantOp
                [(binderName, intType)]
                (T.Tuple [T.Var occurrence intType Nothing, var y])
                (var i)
                Nothing

        case run [("y", var i)] quantified of
          T.Quant _ [(binder, _)] (T.Tuple [T.Var bound _ _, _]) _ _ -> do
            rangeOfName binder @?= rangeOfName binderName
            rangeOfName bound @?= rangeOfName occurrence
          result -> assertFailure ("unexpected result: " <> show result),
      testCase "substitution is simultaneous, not sequential" $
        run [("x", var y), ("y", var x)] (T.Tuple [var x, var y])
          @?= T.Tuple [var y, var x],
      testCase "a name assigned more than once is rejected" $ do
        outcome <- try (evaluate (run [("x", one), ("x", two)] (var x)))

        case outcome of
          Left (ErrorCall _) -> pure ()
          Right result -> assertFailure ("expected an error, got: " <> show result)
    ]
  where
    run assignments expr = evalState (substExpr assignments expr) (0 :: Int)

    runIn scopes assignments expr =
      case runWP (substExpr assignments expr) (Map.empty, scopes) 0 of
        Right (result, _, _) -> Right result
        Left err -> Left (show err)

    x = Name "x" Nothing
    x0 = Name "x0" Nothing
    y = Name "y" Nothing
    i = Name "i" Nothing
    just = Name "Just" Nothing
    var name = T.Var name intType Nothing
    one = T.Lit (A.Num 1) intType Nothing
    two = T.Lit (A.Num 2) intType Nothing
    scrutinee = T.Var (Name "c" Nothing) intType Nothing
    caseOf s pattern' body = T.Case s [T.CaseClause pattern' body] Nothing
    quantOp = T.Var (Name "op" Nothing) intType Nothing
    quantOf binder range body = T.Quant quantOp [(binder, intType)] range body Nothing
    rangeOfName (Name _ range) = range
    intType = A.TBase A.TInt Nothing
