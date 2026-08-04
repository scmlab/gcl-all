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
  ( descend,
    initRZ,
    isRedex,
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
