{-# LANGUAGE OverloadedStrings #-}

module Test.Type (tests) where

import Control.Monad.State (evalState)
import qualified Data.Map as Map
import GCL.Common (Free (freeVars))
import GCL.Range (mkPos, mkRange)
import GCL.Type2.Subst (applySubst)
import GCL.Type2.Types (typeToType)
import Pretty (toText)
import qualified Syntax.Abstract.Operator as AO
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (ChainOp (..), Name (..), Op (..), TypeOp (..))
import Syntax.Typed.Instances.Free ()
import qualified Syntax.Typed.Operator as TO
import Syntax.Typed.Reduce
  ( DefinitionSubstPolicy (..),
    descend,
    initRZ,
    isRedex,
    mustDeferSubstitution,
    redexRT,
    redexRT_sat,
    reduce,
  )
import Syntax.Typed.Reduce.Saturation (saturatedRedex)
import qualified Syntax.Typed.Types as T
import Syntax.Typed.Util (codomain, typeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Type"
    [ testCase "mkArrowType constructs Arrow TApp" $
        assertArrowType (A.mkArrowType intType boolType),
      testCase "tFunc constructs Arrow TApp" $
        assertArrowType (AO.tFunc intType boolType),
      testCase "typeToType uses the canonical Arrow representation" $
        typeToType intType boolType @?= A.mkArrowType intType boolType,
      testCase "Arrow type equality ignores source ranges" $ do
        let range = mkRange (mkPos 1 1) (mkPos 1 3)
            ranged =
              A.TApp
                (A.TApp (A.TOp (Arrow (Just range))) intType (Just range))
                boolType
                (Just range)
        ranged @?= A.mkArrowType intType boolType,
      testCase "Arrow types with different components are unequal" $ do
        A.mkArrowType boolType boolType /= A.mkArrowType intType boolType @? "different argument types"
        A.mkArrowType intType intType /= A.mkArrowType intType boolType @? "different result types",
      testCase "typeOf lambda application returns its result type" $ do
        let x = Name "x" Nothing
            identity = T.Lam x intType (T.Var x intType Nothing) Nothing
            application = T.App identity (T.Lit (A.Num 1) intType Nothing) Nothing
        typeOf application @?= intType,
      testCase "case-pattern binders are excluded from free variables" $ do
        let patternName = Name "r" Nothing
            freeName = Name "t" Nothing
            rhs =
              TO.lt
                (T.Var patternName intType Nothing)
                (T.Var freeName intType Nothing)
            clause = T.CaseClause (A.PattBinder patternName) rhs

        freeVars clause @?= freeVars (T.Var freeName intType Nothing),
      testCase "typed operator annotations use nested Arrow applications" $
        TO.tBinIntOp
          @?= A.mkArrowType intType (A.mkArrowType intType intType),
      testCase "saturation distinguishes partial and full applications" $ do
        let f = Name "f" Nothing
            functionType = A.mkArrowType intType (A.mkArrowType intType intType)
            function = T.Var f functionType Nothing
            argument = T.Lit (A.Num 1) intType Nothing
            partial = T.App function argument Nothing
            full = T.App partial argument Nothing
        saturatedRedex partial @?= False
        saturatedRedex full @?= True,
      testCase "definition redexes respect lexical shadowing" $ do
        let inv = Name "inv" Nothing
            x = Name "x" Nothing
            b = Name "B" Nothing
            functionType = A.mkArrowType intType boolType
            definition = T.Lam x intType (T.Lit (A.Bol True) boolType Nothing) Nothing
            env = [(inv, definition)]
            call = T.App (T.Var inv functionType Nothing) (T.Var b intType Nothing) Nothing
            range = T.Lit (A.Bol True) boolType Nothing
            quant = T.Quant range [(inv, functionType)] range call Nothing
            lambda = T.Lam inv functionType call Nothing
            clause = T.CaseClause (A.PattBinder inv) call
            caseExpr = T.Case (T.Var b functionType Nothing) [clause] Nothing
            quantBodyRZ = descend (initRZ [] (redexRT_sat env quant)) !! 1
            lambdaBodyRZ =
              case descend (initRZ [] (redexRT_sat env lambda)) of
                [bodyRZ] -> bodyRZ
                _ -> error "lambda redex tree should have one child"
            caseBodyRZ = descend (initRZ [] (redexRT_sat env caseExpr)) !! 1

        isRedex (initRZ [] (redexRT_sat env call))
          @? "a global definition call is a redex"
        isRedex (initRZ [] (redexRT_sat [] call)) @?= False
        isRedex quantBodyRZ @?= False
        isRedex lambdaBodyRZ @?= False
        isRedex caseBodyRZ @?= False
        evalState (reduce env call []) (0 :: Int)
          @?= T.Lit (A.Bol True) boolType Nothing
        evalState (reduce env quant [1]) (0 :: Int) @?= quant
        evalState (reduce env lambda [0]) (0 :: Int) @?= lambda
        evalState (reduce env caseExpr [1]) (0 :: Int) @?= caseExpr,
      testCase "shadowing preserves unrelated definition redexes" $ do
        let inv = Name "inv" Nothing
            helper = Name "helper" Nothing
            x = Name "x" Nothing
            b = Name "B" Nothing
            functionType = A.mkArrowType intType boolType
            result = T.Lit (A.Bol True) boolType Nothing
            definition = T.Lam x intType result Nothing
            env = [(inv, definition), (helper, definition)]
            helperCall = T.App (T.Var helper functionType Nothing) (T.Var b intType Nothing) Nothing
            range = T.Lit (A.Bol True) boolType Nothing
            quant = T.Quant range [(inv, functionType)] range helperCall Nothing
            lambda = T.Lam inv functionType helperCall Nothing
            clause = T.CaseClause (A.PattBinder inv) helperCall
            caseExpr = T.Case (T.Var b functionType Nothing) [clause] Nothing
            quantBodyRZ = descend (initRZ [] (redexRT_sat env quant)) !! 1
            lambdaBodyRZ = descend (initRZ [] (redexRT_sat env lambda)) !! 0
            caseBodyRZ = descend (initRZ [] (redexRT_sat env caseExpr)) !! 1

        isRedex quantBodyRZ @? "quantifier shadowing must preserve helper"
        isRedex lambdaBodyRZ @? "lambda shadowing must preserve helper"
        isRedex caseBodyRZ @? "case-clause shadowing must preserve helper"
        evalState (reduce env quant [1]) (0 :: Int)
          @?= T.Quant range [(inv, functionType)] range result Nothing
        evalState (reduce env lambda [0]) (0 :: Int)
          @?= T.Lam inv functionType result Nothing
        evalState (reduce env caseExpr [1]) (0 :: Int)
          @?= T.Case (T.Var b functionType Nothing) [T.CaseClause (A.PattBinder inv) result] Nothing,
      testCase "definition unfolding avoids case-pattern capture in either order" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            c = Name "C" Nothing
            env = [(inv, captureDefinition parameter globalR)]
            call = definitionCall inv (intVar b)
            original =
              T.Case
                (intVar c)
                [T.CaseClause (A.PattBinder globalR) call]
                Nothing
            expected = TO.lt (intVar globalR) (intVar b)
            expandedFirst = evalState (reduce env original [1]) (0 :: Int)
            caseFirst = evalState (reduce env original []) (0 :: Int)
            caseThenExpand = evalState (reduce env caseFirst []) (0 :: Int)

        case expandedFirst of
          T.Case _ [T.CaseClause (A.PattBinder freshR) rhs] _ -> do
            freshR /= globalR @? "the case binder must be alpha-renamed"
            rhs @?= expected
          other -> assertFailure $ "expected one alpha-renamed case clause, got: " <> show other

        evalState (reduce env expandedFirst []) (0 :: Int) @?= expected
        caseThenExpand @?= expected,
      testCase "quantifier alpha-renaming preserves caller argument and definition global" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            expectedFresh = Name "r_0" Nothing
            env = [(inv, captureDefinition parameter globalR)]
            callerR = intVar globalR
            original =
              T.Quant
                callerR
                [(globalR, intType)]
                trueExpr
                (definitionCall inv callerR)
                Nothing
            expanded = evalState (reduce env original [1]) (0 :: Int)

        case expanded of
          T.Quant (T.Var opName _ _) [(freshR, _)] _ body _ -> do
            freshR @?= expectedFresh
            opName @?= freshR
            body @?= TO.lt (intVar globalR) (intVar freshR)
          other -> assertFailure $ "expected one alpha-renamed quantifier, got: " <> show other,
      testCase "lambda alpha-renaming updates bound occurrences" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            env = [(inv, captureDefinition parameter globalR)]
            callerPredicate = TO.lt (intVar globalR) (intVar b)
            original =
              T.Lam
                globalR
                intType
                (TO.conj callerPredicate (definitionCall inv (intVar b)))
                Nothing
            expanded = evalState (reduce env original [0, 1]) (0 :: Int)

        case expanded of
          T.Lam freshR _ body _ -> do
            freshR /= globalR @? "the lambda binder must be alpha-renamed"
            body
              @?= TO.conj
                (TO.lt (intVar freshR) (intVar b))
                (TO.lt (intVar globalR) (intVar b))
          other -> assertFailure $ "expected an alpha-renamed lambda, got: " <> show other,
      testCase "all conflicting binders on the redex path are alpha-renamed" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            c = Name "C" Nothing
            env = [(inv, captureDefinition parameter globalR)]
            original =
              T.Quant
                trueExpr
                [(globalR, intType)]
                trueExpr
                ( T.Case
                    (intVar c)
                    [ T.CaseClause
                        (A.PattBinder globalR)
                        (definitionCall inv (intVar b))
                    ]
                    Nothing
                )
                Nothing
            expanded = evalState (reduce env original [1, 1]) (0 :: Int)

        case expanded of
          T.Quant _ [(outerR, _)] _ (T.Case _ [T.CaseClause (A.PattBinder innerR) rhs] _) _ -> do
            outerR /= globalR @? "the outer quantifier binder must be renamed"
            innerR /= globalR @? "the inner pattern binder must be renamed"
            outerR /= innerR @? "nested binders need distinct fresh names"
            rhs @?= TO.lt (intVar globalR) (intVar b)
          other -> assertFailure $ "expected nested alpha-renamed binders, got: " <> show other,
      testCase "unrelated caller binder is not alpha-renamed" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            callerX = Name "x" Nothing
            env = [(inv, captureDefinition parameter globalR)]
            original =
              T.Quant
                trueExpr
                [(callerX, intType)]
                trueExpr
                (definitionCall inv (intVar callerX))
                Nothing
            expected =
              T.Quant
                trueExpr
                [(callerX, intType)]
                trueExpr
                (TO.lt (intVar globalR) (intVar callerX))
                Nothing

        evalState (reduce env original [1]) (0 :: Int) @?= expected,
      testCase "definition free-name closure protects point-free dependencies" $ do
        let f = Name "F" Nothing
            g = Name "G" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            env =
              [ (f, T.Var g intToBool Nothing),
                (g, captureDefinition parameter globalR)
              ]
            original =
              T.Quant
                trueExpr
                [(globalR, intType)]
                trueExpr
                (definitionCall f (intVar b))
                Nothing
            expanded = evalState (reduce env original [1]) (0 :: Int)

        case expanded of
          T.Quant _ [(freshR, _)] _ body _ -> do
            freshR /= globalR @? "a transitive definition free name must trigger renaming"
            body @?= TO.lt (intVar globalR) (intVar b)
          other -> assertFailure $ "expected a protected point-free expansion, got: " <> show other,
      testCase "alpha-renaming skips fresh names already present in the predicate" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            occupied = Name "r_0" Nothing
            expectedFresh = Name "r_1" Nothing
            env = [(inv, captureDefinition parameter globalR)]
            occupiedPredicate = TO.lt (intVar occupied) (intVar b)
            original =
              T.Quant
                trueExpr
                [(globalR, intType)]
                trueExpr
                (TO.conj occupiedPredicate (definitionCall inv (intVar b)))
                Nothing
            expanded = evalState (reduce env original [1, 1]) (0 :: Int)

        case expanded of
          T.Quant _ [(freshR, _)] _ body _ -> do
            freshR @?= expectedFresh
            body
              @?= TO.conj
                occupiedPredicate
                (TO.lt (intVar globalR) (intVar b))
          other -> assertFailure $ "expected a collision-free fresh binder, got: " <> show other,
      testCase "definition lookup follows Chain, quantifier-range, and Subst paths" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            x = Name "x" Nothing
            env = [(inv, captureDefinition parameter globalR)]
            call = definitionCall inv (intVar b)
            expected = TO.lt (intVar globalR) (intVar b)
            chain = T.Chain (T.More (T.Pure trueExpr) (ChainOp (EQProp Nothing)) boolType call)
            quantRange =
              T.Quant trueExpr [(globalR, intType)] call trueExpr Nothing
            substitution = T.Subst trueExpr [(x, call)]

        evalState (reduce env chain [0]) (0 :: Int)
          @?= T.Chain (T.More (T.Pure trueExpr) (ChainOp (EQProp Nothing)) boolType expected)
        case evalState (reduce env quantRange [0]) (0 :: Int) of
          T.Quant _ [(freshR, _)] range _ _ -> do
            freshR /= globalR @? "the quantifier range is inside binder scope"
            range @?= expected
          other -> assertFailure $ "expected a reduced quantifier range, got: " <> show other
        evalState (reduce env substitution [1]) (0 :: Int)
          @?= T.Subst trueExpr [(x, expected)],
      testCase "case-pattern locals in a definition are not closure free names" $ do
        let f = Name "F" Nothing
            parameter = Name "t" Nothing
            localR = Name "r" Nothing
            b = Name "B" Nothing
            definition =
              T.Lam
                parameter
                intType
                ( T.Case
                    (intVar parameter)
                    [T.CaseClause (A.PattBinder localR) (intVar localR)]
                    Nothing
                )
                Nothing
            env = [(f, definition)]
            original =
              T.Quant
                trueExpr
                [(localR, intType)]
                trueExpr
                (definitionCall f (intVar b))
                Nothing
            expectedBody =
              T.Case
                (intVar b)
                [T.CaseClause (A.PattBinder localR) (intVar localR)]
                Nothing

        evalState (reduce env original [1]) (0 :: Int)
          @?= T.Quant trueExpr [(localR, intType)] trueExpr expectedBody Nothing,
      testCase "definition Const free names rename the caller binder and stay global" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            globalConst = T.Const globalR intType Nothing
            definition =
              T.Lam
                parameter
                intType
                (TO.lt globalConst (intVar parameter))
                Nothing
            env = [(inv, definition)]
            original =
              T.Quant
                trueExpr
                [(globalR, intType)]
                trueExpr
                (definitionCall inv (intVar globalR))
                Nothing
            expanded = evalState (reduce env original [1]) (0 :: Int)

        case expanded of
          T.Quant _ [(freshR, _)] _ body _ -> do
            freshR /= globalR @? "a Const free name in the definition must trigger renaming"
            body @?= TO.lt globalConst (intVar freshR)
          other -> assertFailure $ "expected a renamed binder around a Const, got: " <> show other,
      testCase "issue 179 substitution remains pending until definition unfolding" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            replacement = TO.add (intVar globalR) (TO.number 1)
            table = [(globalR, replacement)]
            env = [(inv, captureDefinition parameter globalR)]
            call = definitionCall inv (intVar b)
            original = T.Subst call table
            pending =
              T.App
                (T.Subst (T.Var inv intToBool Nothing) table)
                (intVar b)
                Nothing
            expected = TO.lt replacement (intVar b)
            substitutionFirst = evalState (reduce env original []) (0 :: Int)
            pendingExpanded = evalState (reduce env substitutionFirst [0]) (0 :: Int)
            substitutionResult = evalState (reduce env pendingExpanded []) (0 :: Int)
            definitionFirst = evalState (reduce env original [0]) (0 :: Int)
            definitionResult = evalState (reduce env definitionFirst []) (0 :: Int)

        substitutionFirst @?= pending
        substitutionResult @?= expected
        definitionResult @?= expected,
      testCase "irrelevant substitution does not wrap a definition" $ do
        let f = Name "F" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            definition = T.Lam parameter intType trueExpr Nothing
            env = [(f, definition)]
            call = definitionCall f (intVar b)
            original = T.Subst call [(globalR, TO.number 1)]

        evalState (reduce env original []) (0 :: Int) @?= call,
      testCase "pending substitution follows point-free definition closure" $ do
        let f = Name "F" Nothing
            g = Name "G" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            replacement = TO.number 5
            table = [(globalR, replacement)]
            env =
              [ (f, T.Var g intToBool Nothing),
                (g, captureDefinition parameter globalR)
              ]
            original = T.Subst (definitionCall f (intVar b)) table
            afterOuter = evalState (reduce env original []) (0 :: Int)
            afterF = evalState (reduce env afterOuter [0]) (0 :: Int)
            expectedAfterF =
              T.App
                (T.Subst (T.Var g intToBool Nothing) table)
                (intVar b)
                Nothing

        afterF @?= expectedAfterF,
      testCase "pending closure uses the global env below a shadowing lambda" $ do
        let f = Name "F" Nothing
            g = Name "G" Nothing
            freshG = Name "G_0" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            replacement = TO.number 5
            table = [(globalR, replacement)]
            env =
              [ (f, T.Var g intToBool Nothing),
                (g, captureDefinition parameter globalR)
              ]
            caller =
              T.Lam
                g
                intToBool
                (definitionCall f (intVar b))
                Nothing
            original = T.Subst caller table
            expectedPending =
              T.Lam
                g
                intToBool
                ( T.App
                    (T.Subst (T.Var f intToBool Nothing) table)
                    (intVar b)
                    Nothing
                )
                Nothing
            expectedAfterF =
              T.Lam
                freshG
                intToBool
                ( T.App
                    (T.Subst (T.Var g intToBool Nothing) table)
                    (intVar b)
                    Nothing
                )
                Nothing
            expected =
              T.Lam
                freshG
                intToBool
                (TO.lt replacement (intVar b))
                Nothing
            substitutionFirst = evalState (reduce env original []) (0 :: Int)
            afterF = evalState (reduce env substitutionFirst [0, 0]) (0 :: Int)
            afterG = evalState (reduce env afterF [0, 0]) (0 :: Int)
            substitutionResult = evalState (reduce env afterG [0]) (0 :: Int)
            definitionFirst = evalState (reduce env original [0, 0]) (0 :: Int)
            definitionResult = evalState (reduce env definitionFirst []) (0 :: Int)

        substitutionFirst @?= expectedPending
        afterF @?= expectedAfterF
        substitutionResult @?= expected
        definitionResult @?= expected,
      testCase "pending closure uses the global env below a shadowing quantifier" $ do
        let f = Name "F" Nothing
            g = Name "G" Nothing
            freshG = Name "G_0" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            replacement = TO.number 5
            table = [(globalR, replacement)]
            env =
              [ (f, T.Var g intToBool Nothing),
                (g, captureDefinition parameter globalR)
              ]
            quantified =
              T.Quant
                trueExpr
                [(g, intToBool)]
                trueExpr
                (definitionCall f (intVar b))
                Nothing
            original = T.Subst quantified table
            expectedPending =
              T.Quant
                trueExpr
                [(g, intToBool)]
                trueExpr
                ( T.App
                    (T.Subst (T.Var f intToBool Nothing) table)
                    (intVar b)
                    Nothing
                )
                Nothing
            expectedAfterF =
              T.Quant
                trueExpr
                [(freshG, intToBool)]
                trueExpr
                ( T.App
                    (T.Subst (T.Var g intToBool Nothing) table)
                    (intVar b)
                    Nothing
                )
                Nothing
            expected =
              T.Quant
                trueExpr
                [(freshG, intToBool)]
                trueExpr
                (TO.lt replacement (intVar b))
                Nothing
            substitutionFirst = evalState (reduce env original []) (0 :: Int)
            afterF = evalState (reduce env substitutionFirst [1, 0]) (0 :: Int)
            afterG = evalState (reduce env afterF [1, 0]) (0 :: Int)
            substitutionResult = evalState (reduce env afterG [1]) (0 :: Int)
            definitionFirst = evalState (reduce env original [0, 1]) (0 :: Int)
            definitionResult = evalState (reduce env definitionFirst []) (0 :: Int)

        substitutionFirst @?= expectedPending
        afterF @?= expectedAfterF
        substitutionResult @?= expected
        definitionResult @?= expected,
      testCase "same-domain quantified binder retains opaque substitution" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            freshR = Name "r_0" Nothing
            replacement = TO.add (intVar globalR) (TO.number 1)
            table = [(globalR, replacement)]
            env = [(inv, captureDefinition parameter globalR)]
            quantified =
              T.Quant
                trueExpr
                [(globalR, intType)]
                trueExpr
                (definitionCall inv (intVar globalR))
                Nothing
            original = T.Subst quantified table
            expectedPending =
              T.Quant
                trueExpr
                [(freshR, intType)]
                trueExpr
                ( T.App
                    (T.Subst (T.Var inv intToBool Nothing) table)
                    (intVar freshR)
                    Nothing
                )
                Nothing
            expected =
              T.Quant
                trueExpr
                [(freshR, intType)]
                trueExpr
                (TO.lt replacement (intVar freshR))
                Nothing
            substitutionFirst = evalState (reduce env original []) (0 :: Int)
            pendingExpanded = evalState (reduce env substitutionFirst [1, 0]) (0 :: Int)
            substitutionResult = evalState (reduce env pendingExpanded [1]) (0 :: Int)
            definitionFirst = evalState (reduce env original [0, 1]) (0 :: Int)
            definitionResult = evalState (reduce env definitionFirst []) (0 :: Int)

        substitutionFirst @?= expectedPending
        substitutionResult @?= expected
        definitionResult @?= expected,
      testCase "transparent same-domain binder keeps its source name" $ do
        let globalR = Name "r" Nothing
            quantified =
              T.Quant
                trueExpr
                [(globalR, intType)]
                trueExpr
                (TO.lt (intVar globalR) (TO.number 0))
                Nothing
            original =
              T.Subst
                quantified
                [(globalR, TO.add (intVar globalR) (TO.number 1))]

        evalState (reduce [] original []) (0 :: Int) @?= quantified,
      testCase "irrelevant transparent substitution avoids cosmetic alpha-renaming" $ do
        let x = Name "x" Nothing
            y = Name "y" Nothing
            z = Name "z" Nothing
            lambda = T.Lam x intType (intVar z) Nothing
            original = T.Subst lambda [(y, intVar x)]

        evalState (reduce [] original []) (0 :: Int) @?= lambda,
      testCase "opacity-conflicted nested substitution waits for its inner child" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            d = Name "d" Nothing
            x = Name "x" Nothing
            b = Name "B" Nothing
            five = TO.number 5
            definition =
              T.Lam
                parameter
                intType
                (TO.add (intVar d) (TO.add (intVar x) (intVar parameter)))
                Nothing
            env = [(inv, definition)]
            call = definitionCall inv (intVar b)
            inner = T.Subst call [(d, five)]
            outerTable = [(x, intVar d)]
            original = T.Subst inner outerTable
            expected = TO.add five (TO.add (intVar d) (intVar b))
            root tree = isRedex (initRZ [] tree)

        mustDeferSubstitution SuspendAtDefinitions env outerTable inner
          @? "the outer substitution must detect the opaque capture conflict"
        root (redexRT env original) @?= False
        root (redexRT_sat env original) @?= False
        case descend (initRZ [] (redexRT_sat env original)) of
          innerRZ : _ -> isRedex innerRZ @? "the inner substitution remains selectable"
          _ -> assertFailure "nested substitution redex tree has no subject child"
        evalState (reduce env original []) (0 :: Int) @?= original

        let innerFirst = evalState (reduce env original [0]) (0 :: Int)
            outerApplied = evalState (reduce env innerFirst []) (0 :: Int)
            innerPendingExpanded = evalState (reduce env outerApplied [0, 0]) (0 :: Int)
            outerPendingApplied = evalState (reduce env innerPendingExpanded [0]) (0 :: Int)
            deferredResult = evalState (reduce env outerPendingApplied []) (0 :: Int)
            definitionFirst = evalState (reduce env original [0, 0]) (0 :: Int)
            definitionThenOuter = evalState (reduce env definitionFirst []) (0 :: Int)
            definitionResult = evalState (reduce env definitionThenOuter []) (0 :: Int)

        deferredResult @?= expected
        definitionResult @?= expected,
      testCase "beta reduction defers an opacity-conflicted nested substitution" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            d = Name "d" Nothing
            x = Name "x" Nothing
            five = TO.number 5
            definition =
              T.Lam
                parameter
                intType
                (TO.add (intVar d) (intVar parameter))
                Nothing
            env = [(inv, definition)]
            body = T.Subst (definitionCall inv (intVar x)) [(d, five)]
            original =
              T.App
                (T.Lam x intType body Nothing)
                (intVar d)
                Nothing
            root tree = isRedex (initRZ [] tree)

        root (redexRT env original) @?= False
        root (redexRT_sat env original) @?= False
        evalState (reduce env original []) (0 :: Int) @?= original

        let innerFirst = evalState (reduce env original [0, 0]) (0 :: Int)
            expectedBody =
              T.App
                (T.Subst (T.Var inv intToBool Nothing) [(d, five)])
                (intVar x)
                Nothing
            expectedInnerFirst =
              T.App
                (T.Lam x intType expectedBody Nothing)
                (intVar d)
                Nothing
            expectedAfterBeta =
              T.App
                (T.Subst (T.Var inv intToBool Nothing) [(d, five)])
                (intVar d)
                Nothing

        innerFirst @?= expectedInnerFirst
        root (redexRT_sat env innerFirst) @? "beta becomes selectable after the inner step"
        evalState (reduce env innerFirst []) (0 :: Int) @?= expectedAfterBeta,
      testCase "case deferral preserves the first matching clause" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            d = Name "d" Nothing
            x = Name "x" Nothing
            fallback = Name "fallback" Nothing
            five = TO.number 5
            definition =
              T.Lam
                parameter
                intType
                (TO.add (intVar d) (intVar parameter))
                Nothing
            env = [(inv, definition)]
            firstRhs = T.Subst (definitionCall inv (intVar x)) [(d, five)]
            original =
              T.Case
                (intVar d)
                [ T.CaseClause (A.PattBinder x) firstRhs,
                  T.CaseClause (A.PattBinder fallback) trueExpr
                ]
                Nothing
            root tree = isRedex (initRZ [] tree)

        root (redexRT env original) @?= False
        root (redexRT_sat env original) @?= False
        evalState (reduce env original []) (0 :: Int) @?= original

        let innerFirst = evalState (reduce env original [1]) (0 :: Int)
            expectedFirstRhs =
              T.App
                (T.Subst (T.Var inv intToBool Nothing) [(d, five)])
                (intVar x)
                Nothing
            expectedInnerFirst =
              T.Case
                (intVar d)
                [ T.CaseClause (A.PattBinder x) expectedFirstRhs,
                  T.CaseClause (A.PattBinder fallback) trueExpr
                ]
                Nothing
            expectedAfterCase =
              T.App
                (T.Subst (T.Var inv intToBool Nothing) [(d, five)])
                (intVar d)
                Nothing

        innerFirst @?= expectedInnerFirst
        root (redexRT_sat env innerFirst) @? "case becomes selectable after the inner step"
        evalState (reduce env innerFirst []) (0 :: Int) @?= expectedAfterCase,
      testCase "beta preflight drops substitutions irrelevant below a transparent binder" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            d = Name "d" Nothing
            x = Name "x" Nothing
            z = Name "z" Nothing
            b = Name "B" Nothing
            five = TO.number 5
            env = [(inv, captureDefinition parameter d)]
            inner =
              T.Subst
                (definitionCall inv (intVar b))
                [(d, five)]
            quantified =
              T.Quant trueExpr [(z, intType)] trueExpr inner Nothing
            original =
              T.App
                (T.Lam x intType quantified Nothing)
                (intVar d)
                Nothing
            root tree = isRedex (initRZ [] tree)

        root (redexRT env original)
          @? "the irrelevant beta substitution must not defer"
        root (redexRT_sat env original)
          @? "saturated marking must mirror beta traversal filtering"
        evalState (reduce env original []) (0 :: Int) @?= quantified,
      testCase "beta preflight drops irrelevant substitutions before opaque nested subjects" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            d = Name "d" Nothing
            x = Name "x" Nothing
            b = Name "B" Nothing
            five = TO.number 5
            env = [(inv, captureDefinition parameter d)]
            inner =
              T.Subst
                (definitionCall inv (intVar b))
                [(d, five)]
            original =
              T.App
                (T.Lam x intType inner Nothing)
                (intVar d)
                Nothing
            root tree = isRedex (initRZ [] tree)

        root (redexRT env original)
          @? "an unused beta substitution must not wait for the nested subject"
        root (redexRT_sat env original)
          @? "saturated beta marking must use the policy-aware subject table"
        evalState (reduce env original []) (0 :: Int) @?= inner,
      testCase "case preflight drops substitutions irrelevant below a transparent binder" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            d = Name "d" Nothing
            x = Name "x" Nothing
            z = Name "z" Nothing
            b = Name "B" Nothing
            fallback = Name "fallback" Nothing
            five = TO.number 5
            env = [(inv, captureDefinition parameter d)]
            inner =
              T.Subst
                (definitionCall inv (intVar b))
                [(d, five)]
            quantified =
              T.Quant trueExpr [(z, intType)] trueExpr inner Nothing
            original =
              T.Case
                (intVar d)
                [ T.CaseClause (A.PattBinder x) quantified,
                  T.CaseClause
                    (A.PattBinder fallback)
                    (T.Lit (A.Bol False) boolType Nothing)
                ]
                Nothing
            root tree = isRedex (initRZ [] tree)

        root (redexRT env original)
          @? "the irrelevant case substitution must not defer"
        root (redexRT_sat env original)
          @? "saturated marking must mirror case traversal filtering"
        evalState (reduce env original []) (0 :: Int) @?= quantified,
      testCase "case preflight drops irrelevant substitutions before opaque nested subjects" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            d = Name "d" Nothing
            x = Name "x" Nothing
            b = Name "B" Nothing
            fallback = Name "fallback" Nothing
            five = TO.number 5
            env = [(inv, captureDefinition parameter d)]
            inner =
              T.Subst
                (definitionCall inv (intVar b))
                [(d, five)]
            original =
              T.Case
                (intVar d)
                [ T.CaseClause (A.PattBinder x) inner,
                  T.CaseClause
                    (A.PattBinder fallback)
                    (T.Lit (A.Bol False) boolType Nothing)
                ]
                Nothing
            root tree = isRedex (initRZ [] tree)

        root (redexRT env original)
          @? "an unused case substitution must not wait for the nested subject"
        root (redexRT_sat env original)
          @? "saturated case marking must use the policy-aware subject table"
        evalState (reduce env original []) (0 :: Int) @?= inner,
      testCase "beta substitution preserves pending domains and updates their ranges" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            pendingDomain = Name "r" Nothing
            betaBinder = Name "x" Nothing
            five = TO.number 5
            env = [(inv, captureDefinition parameter pendingDomain)]
            pending =
              T.Subst
                (T.Var inv intToBool Nothing)
                [(pendingDomain, intVar betaBinder)]
            body = T.App pending (intVar betaBinder) Nothing
            original =
              T.App
                (T.Lam betaBinder intType body Nothing)
                five
                Nothing
            expected =
              T.App
                ( T.Subst
                    (T.Var inv intToBool Nothing)
                    [(pendingDomain, five)]
                )
                five
                Nothing

        evalState (reduce env original []) (0 :: Int) @?= expected,
      testCase "application fallback does not unfold a pending function" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            five = TO.number 5
            env = [(inv, captureDefinition parameter globalR)]
            pending =
              T.Subst
                (T.Var inv intToBool Nothing)
                [(globalR, five)]
            original = T.App pending (intVar b) Nothing

        evalState (reduce env original []) (0 :: Int) @?= original
        evalState (reduce env original [0]) (0 :: Int)
          @?= T.App
            ( T.Lam
                parameter
                intType
                (TO.lt five (intVar parameter))
                Nothing
            )
            (intVar b)
            Nothing,
      testCase "one pending step does not recursively unfold a recursive definition" $ do
        let f = Name "F" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            five = TO.number 5
            recursiveCall = definitionCall f (intVar parameter)
            definition =
              T.Lam
                parameter
                intType
                (TO.add (intVar globalR) recursiveCall)
                Nothing
            env = [(f, definition)]
            table = [(globalR, five)]
            original = T.Subst (definitionCall f (intVar b)) table
            afterOuter = evalState (reduce env original []) (0 :: Int)
            afterPending = evalState (reduce env afterOuter [0]) (0 :: Int)
            expectedRecursive =
              T.Subst
                (T.Var f intToBool Nothing)
                table

        case afterPending of
          T.App (T.Lam _ _ (T.App (T.App _ left _) right _) _) _ _ -> do
            left @?= five
            right @?= T.App expectedRecursive (intVar parameter) Nothing
          other -> assertFailure $ "expected one unfolded layer with a pending recursive call, got: " <> show other,
      testCase "one pending step does not recursively unfold mutual recursion" $ do
        let f = Name "F" Nothing
            g = Name "G" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            five = TO.number 5
            intToInt = A.mkArrowType intType intType
            call name argument =
              T.App (T.Var name intToInt Nothing) argument Nothing
            fDefinition =
              T.Lam
                parameter
                intType
                (TO.add (intVar globalR) (call g (intVar parameter)))
                Nothing
            gDefinition =
              T.Lam
                parameter
                intType
                (call f (intVar parameter))
                Nothing
            env = [(f, fDefinition), (g, gDefinition)]
            table = [(globalR, five)]
            original = T.Subst (call f (intVar b)) table
            afterOuter = evalState (reduce env original []) (0 :: Int)
            afterPending = evalState (reduce env afterOuter [0]) (0 :: Int)
            expected =
              T.App
                ( T.Lam
                    parameter
                    intType
                    ( TO.add
                        five
                        ( T.App
                            (T.Subst (T.Var g intToInt Nothing) table)
                            (intVar parameter)
                            Nothing
                        )
                    )
                    Nothing
                )
                (intVar b)
                Nothing

        afterPending @?= expected,
      testCase "defensive pending conflict cancels the whole reduction" $ do
        let f = Name "F" Nothing
            inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            d = Name "d" Nothing
            x = Name "x" Nothing
            y = Name "y" Nothing
            b = Name "B" Nothing
            five = TO.number 5
            conflicted =
              T.Subst
                (definitionCall inv (intVar b))
                [(d, five)]
            definition = T.Tuple [intVar x, conflicted]
            table = [(x, five), (y, intVar d)]
            env =
              [ (f, definition),
                (inv, captureDefinition parameter d)
              ]
            original = T.Subst (T.Var f intType Nothing) table

        evalState (reduce env original []) (0 :: Int) @?= original,
      testCase "ordinary nested substitution keeps classical transparent renaming" $ do
        let d = Name "d" Nothing
            x = Name "x" Nothing
            freshD = Name "d_0" Nothing
            five = TO.number 5
            inner = T.Subst (TO.add (intVar d) (intVar x)) [(d, five)]
            original = T.Subst inner [(x, intVar d)]
            expected =
              T.Subst
                (TO.add (intVar freshD) (intVar d))
                [(freshD, five)]

        mustDeferSubstitution SuspendAtDefinitions [] [(x, intVar d)] inner
          @?= False
        evalState (reduce [] original []) (0 :: Int) @?= expected,
      testCase "transparent nested substitution avoids irrelevant domain renaming" $ do
        let r = Name "r" Nothing
            x = Name "x" Nothing
            y = Name "y" Nothing
            inner =
              T.Subst
                (intVar r)
                [(r, intVar x), (y, intVar x)]
            original = T.Subst inner [(x, intVar y)]
            expected =
              T.Subst
                (intVar r)
                [(r, intVar y), (y, intVar y)]

        evalState (reduce [] original []) (0 :: Int) @?= expected,
      testCase "ordinary nested substitution drops overlapping outer domains" $ do
        let x = Name "x" Nothing
            y = Name "y" Nothing
            seven = TO.number 7
            nine = TO.number 9
            inner =
              T.Subst
                (TO.add (intVar x) (intVar y))
                [(x, intVar y)]
            original = T.Subst inner [(x, nine), (y, seven)]
            expectedAfterOuter =
              T.Subst
                (TO.add (intVar x) seven)
                [(x, seven)]
            expected = TO.add seven seven
            afterOuter = evalState (reduce [] original []) (0 :: Int)

        afterOuter @?= expectedAfterOuter
        evalState (reduce [] afterOuter []) (0 :: Int) @?= expected,
      testCase "stacked pending substitutions reduce inner to outer" $ do
        let f = Name "F" Nothing
            r = Name "r" Nothing
            x = Name "x" Nothing
            y = Name "y" Nothing
            env = [(f, intVar r)]
            inner = T.Subst (T.Var f intType Nothing) [(r, intVar x)]
            original = T.Subst inner [(x, intVar y)]
            root tree = isRedex (initRZ [] tree)

        root (redexRT env original) @?= False
        root (redexRT_sat env original) @?= False
        evalState (reduce env original []) (0 :: Int) @?= original

        let innerFirst = evalState (reduce env original [0]) (0 :: Int)
        innerFirst @?= T.Subst (intVar x) [(x, intVar y)]
        root (redexRT_sat env innerFirst) @? "the outer layer becomes reducible"
        evalState (reduce env innerFirst []) (0 :: Int) @?= intVar y,
      testCase "definition-aware substitution respects case-pattern binders" $ do
        let f = Name "F" Nothing
            parameter = Name "t" Nothing
            localR = Name "r" Nothing
            x = Name "x" Nothing
            b = Name "B" Nothing
            freshR = Name "r_0" Nothing
            definition =
              T.Lam
                parameter
                intType
                ( T.Case
                    (intVar parameter)
                    [ T.CaseClause
                        (A.PattBinder localR)
                        (TO.add (intVar localR) (intVar x))
                    ]
                    Nothing
                )
                Nothing
            env = [(f, definition)]
            table = [(x, intVar localR)]
            original = T.Subst (definitionCall f (intVar b)) table
            afterOuter = evalState (reduce env original []) (0 :: Int)
            afterPending = evalState (reduce env afterOuter [0]) (0 :: Int)
            expectedBody =
              T.Case
                (intVar parameter)
                [ T.CaseClause
                    (A.PattBinder freshR)
                    (TO.add (intVar freshR) (intVar localR))
                ]
                Nothing

        afterPending
          @?= T.App
            (T.Lam parameter intType expectedBody Nothing)
            (intVar b)
            Nothing,
      testCase "definition-aware substitution respects definition-name shadowing" $ do
        let inv = Name "Inv" Nothing
            parameter = Name "t" Nothing
            globalR = Name "r" Nothing
            b = Name "B" Nothing
            env = [(inv, captureDefinition parameter globalR)]
            localCall = definitionCall inv (intVar b)
            lambda = T.Lam inv intToBool localCall Nothing
            original = T.Subst lambda [(globalR, TO.number 5)]

        evalState (reduce env original []) (0 :: Int) @?= lambda,
      testCase "definition-aware substitution preserves Const and source metadata" $ do
        let nameRange = mkRange (mkPos 1 1) (mkPos 1 2)
            constRange = mkRange (mkPos 1 3) (mkPos 1 4)
            variableRange = mkRange (mkPos 1 5) (mkPos 1 6)
            applicationRange = mkRange (mkPos 1 3) (mkPos 1 6)
            replacementRange = mkRange (mkPos 2 1) (mkPos 2 2)
            constant = Name "c" (Just nameRange)
            variable = Name "x" (Just variableRange)
            constantExpr = T.Const constant intToBool (Just constRange)
            variableExpr = T.Var variable intType (Just variableRange)
            replacement = T.Lit (A.Num 5) intType (Just replacementRange)
            application =
              T.App constantExpr variableExpr (Just applicationRange)
            original = T.Subst application [(variable, replacement)]
            expected =
              T.App constantExpr replacement (Just applicationRange)

        evalState (reduce [] original []) (0 :: Int) @?= expected
        evalState
          (reduce [] (T.Subst constantExpr [(constant, replacement)]) [])
          (0 :: Int)
          @?= replacement,
      testCase "function signatures render as arrows" $
        toText (A.mkArrowType intType boolType) @?= "Int → Bool",
      testCase "substitution traverses nested Arrow applications" $ do
        let a = Name "a" Nothing
            polymorphic = A.mkArrowType (A.TVar a Nothing) (A.mkArrowType intType (A.TVar a Nothing))
            expected = A.mkArrowType boolType (A.mkArrowType intType boolType)
        applySubst (Map.singleton a boolType) polymorphic @?= expected,
      testCase "array codomain remains its element type" $ do
        let endpoint = A.Including (A.Lit (A.Num 0) Nothing)
            arrayType = A.TArray (A.Interval endpoint endpoint Nothing) boolType Nothing
        codomain arrayType @?= boolType
    ]
  where
    assertArrowType actual =
      case actual of
        A.TApp (A.TApp (A.TOp (Arrow _)) argument _) result _ -> do
          argument @?= intType
          result @?= boolType
        other -> assertFailure $ "expected Arrow TApp, got: " <> show other

intType :: A.Type
intType = A.TBase A.TInt Nothing

boolType :: A.Type
boolType = A.TBase A.TBool Nothing

intToBool :: A.Type
intToBool = A.mkArrowType intType boolType

intVar :: Name -> T.Expr
intVar name = T.Var name intType Nothing

trueExpr :: T.Expr
trueExpr = T.Lit (A.Bol True) boolType Nothing

definitionCall :: Name -> T.Expr -> T.Expr
definitionCall name argument =
  T.App (T.Var name intToBool Nothing) argument Nothing

captureDefinition :: Name -> Name -> T.Expr
captureDefinition parameter global =
  T.Lam
    parameter
    intType
    (TO.lt (intVar global) (intVar parameter))
    Nothing
