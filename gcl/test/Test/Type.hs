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
import Syntax.Common.Types (Name (..), TypeOp (..))
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
              T.Tuple
                [ T.Var patternName intType Nothing,
                  T.Var freeName intType Nothing
                ]
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
