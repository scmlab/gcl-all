{-# LANGUAGE OverloadedStrings #-}

module Test.Type (tests) where

import Control.Monad.State (evalState)
import qualified Data.Map as Map
import Data.Text (Text)
import GCL.Common (Free (freeVars))
import GCL.Range (mkPos, mkRange)
import GCL.Type2.Infer (checkDuplicateBinders, infer)
import GCL.Type2.Subst (applySubst)
import GCL.Type2.Types (TypeError (..), mkInference, runTI, typeToType)
import Pretty (toText)
import qualified Syntax.Abstract.Operator as AO
import qualified Syntax.Abstract.Types as A
import qualified Syntax.Common.Types as C
import Syntax.Common.Types (Name (..), TypeOp (..))
import qualified Syntax.Concrete.Instances.ToAbstract as AT
import qualified Syntax.Parser as Parser
import Syntax.Substitution (subst)
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
      testCase "abstract quantifier binders do not scope over the operator" $ do
        let operatorName = Name "i" Nothing
            bodyName = Name "j" Nothing
            operator = A.Var operatorName Nothing
            restriction = A.Var bodyName Nothing
            body = A.Tuple [A.Var operatorName Nothing, A.Var bodyName Nothing]
            quantifier = A.Quant operator [operatorName, bodyName] restriction body Nothing

        freeVars quantifier @?= freeVars operator,
      testCase "typed quantifier binders do not scope over the operator" $ do
        let operatorName = Name "i" Nothing
            bodyName = Name "j" Nothing
            operator = T.Var operatorName intType Nothing
            restriction = T.Var bodyName intType Nothing
            body = T.Tuple [T.Var operatorName intType Nothing, T.Var bodyName intType Nothing]
            binders = [(operatorName, intType), (bodyName, intType)]
            quantifier = T.Quant operator binders restriction body Nothing

        freeVars quantifier @?= freeVars operator,
      -- The first i is the operator; the second is the quantifier binder:
      --   ⟨ i i : i : i ⟩[i := 1] → ⟨ 1 i : i : i ⟩
      testCase "term substitution applies outside quantifier binder scope" $ do
        let binder = Name "i" Nothing
            occurrence = T.Var binder intType Nothing
            replacement = T.Lit (A.Num 1) intType Nothing
            quantifier = T.Quant occurrence [(binder, intType)] occurrence occurrence Nothing
            expected = T.Quant replacement [(binder, intType)] occurrence occurrence Nothing

        evalState (subst [("i", replacement)] quantifier) (0 :: Int) @?= expected,
      -- The operator is outside the binder's scope, so substituting i there does
      -- not require renaming the binder:
      --   ⟨ op i : i : i ⟩[op := i] → ⟨ i i : i : i ⟩
      testCase "operator substitution does not rename quantifier binders" $ do
        let operatorName = Name "op" Nothing
            binder = Name "i" Nothing
            operator = T.Var operatorName intType Nothing
            boundOccurrence = T.Var binder intType Nothing
            replacement = T.Var binder intType Nothing
            quantifier = T.Quant operator [(binder, intType)] boundOccurrence boundOccurrence Nothing
            expected = T.Quant replacement [(binder, intType)] boundOccurrence boundOccurrence Nothing

        evalState (subst [("op", replacement)] quantifier) (0 :: Int) @?= expected,
      testCase "duplicate binders are rejected across patterns" $
        let i = Name "i" Nothing
            j = Name "j" Nothing
         in checkDuplicateBinders
              [A.PattBinder i, A.PattBinder j, A.PattBinder i]
              @?= Left (DuplicatedIdentifiers [i]),
      testCase "duplicate binders in a definition are rejected" $
        inferDefinition "{:\nf x x = x\n:}"
          @?= Left (DuplicatedIdentifiers [Name "x" Nothing]),
      testCase "every duplicated binder is reported" $
        inferDefinition "{:\nf x y x y = x\n:}"
          @?= Left (DuplicatedIdentifiers [Name "x" Nothing, Name "y" Nothing]),
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
            quantBodyRZ = descend (initRZ [] (redexRT_sat env quant)) !! 2
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
        evalState (reduce env quant [2]) (0 :: Int) @?= quant
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
            quantBodyRZ = descend (initRZ [] (redexRT_sat env quant)) !! 2
            lambdaBodyRZ = descend (initRZ [] (redexRT_sat env lambda)) !! 0
            caseBodyRZ = descend (initRZ [] (redexRT_sat env caseExpr)) !! 1

        isRedex quantBodyRZ @? "quantifier shadowing must preserve helper"
        isRedex lambdaBodyRZ @? "lambda shadowing must preserve helper"
        isRedex caseBodyRZ @? "case-clause shadowing must preserve helper"
        evalState (reduce env quant [2]) (0 :: Int)
          @?= T.Quant range [(inv, functionType)] range result Nothing
        evalState (reduce env lambda [0]) (0 :: Int)
          @?= T.Lam inv functionType result Nothing
        evalState (reduce env caseExpr [1]) (0 :: Int)
          @?= T.Case (T.Var b functionType Nothing) [T.CaseClause (A.PattBinder inv) result] Nothing,
      -- Internal-AST equivalent of the following PSEUDO source:
      --
      --   outer f :: Int -> (Bool -> Bool -> Bool)
      --   outer f x = (&&)
      --   ⟨ (f a) f : True : True ⟩
      --     │     └─ binder f, whose scope starts at the restriction
      --     └─ (f a) is the operator, reducible at path [0]
      --     ↓
      --   ⟨ (&&) f : True : True ⟩
      --
      testCase "quantifier operator redex uses outer scope" $ do
        let f = Name "f" Nothing
            x = Name "x" Nothing
            argumentName = Name "a" Nothing
            operatorType = TO.tBinLogicOp
            functionType = A.mkArrowType intType operatorType
            quantifierOperator = T.Op (C.ArithOp (C.ConjU Nothing)) operatorType
            definition = T.Lam x intType quantifierOperator Nothing
            env = [(f, definition)]
            call = T.App (T.Var f functionType Nothing) (T.Var argumentName intType Nothing) Nothing
            restriction = T.Lit (A.Bol True) boolType Nothing
            quantifier = T.Quant call [(f, functionType)] restriction restriction Nothing
            expected = T.Quant quantifierOperator [(f, functionType)] restriction restriction Nothing

        evalState (reduce env quantifier [0]) (0 :: Int) @?= expected,
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
        codomain arrayType @?= boolType,
      -- Internal-AST equivalent of the following PSEUDO source:
      --
      --   outer i :: Int -> Int -> Int
      --   ⟨ i i : True : i ⟩
      --     │ │          └─ bound occurrence
      --     │ └─ binder
      --     └─ operator resolved from the outer environment
      --
      -- The operator i and binder i must resolve to different bindings.
      testCase "quantifier operator is inferred outside binder scope" $ do
        let i = Name "i" Nothing
            operatorType = intType `typeToType` intType `typeToType` intType
            env = Map.singleton i (A.Forall [] operatorType)
            occurrence = A.Var i Nothing
            restriction = A.Lit (A.Bol True) Nothing
            quantifier = A.Quant occurrence [i] restriction occurrence Nothing

        case runTI (infer quantifier) env mkInference of
          Left err -> assertFailure $ "unexpected inference failure: " <> show err
          Right ((_, resultType, T.Quant typedOperator [(_, binderType)] _ _ _), _) -> do
            typeOf typedOperator @?= operatorType
            binderType @?= intType
            resultType @?= intType
          Right ((_, _, typedExpr), _) ->
            assertFailure $ "expected typed quantifier, got: " <> show typedExpr,
      testCase "duplicate quantifier binders are rejected" $
        inferSource "<| + i i : i < 3 : i |>"
          @?= Left (DuplicatedIdentifiers [Name "i" Nothing]),
      testCase "duplicate binders are rejected for the counting quantifier" $
        inferSource "<| # i i : i < 3 : i < 3 |>"
          @?= Left (DuplicatedIdentifiers [Name "i" Nothing]),
      testCase "a duplicate quantifier binder is caught across other binders" $
        inferSource "<| + i j i : i < 3 : i |>"
          @?= Left (DuplicatedIdentifiers [Name "i" Nothing]),
      testCase "distinct quantifier binders are accepted" $
        inferSource "<| + i j : i < 3 : i |>" @?= Right intType
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

-- | Infer the type of an expression written in source syntax, so a test goes
--   through the same path a user's file does.
inferSource :: Text -> Either TypeError A.Type
inferSource source =
  case Parser.scanAndParse Parser.expression "<test>" source of
    Left _ -> error "parse failure in test source"
    Right concrete ->
      case runTI (infer (AT.runAbstractTransform concrete)) mempty mkInference of
        Left err -> Left err
        Right ((_, ty, _), _) -> Right ty

-- | Type-check the body of the first definition in a program's definition
--   block, so a test goes through the same desugaring a user's file does.
inferDefinition :: Text -> Either TypeError A.Type
inferDefinition source =
  case Parser.scanAndParse Parser.program "<test>" source of
    Left _ -> error "parse failure in test source"
    Right concrete ->
      case AT.runAbstractTransform concrete :: A.Program of
        A.Program (A.ValDefn _ _ body : _) _ _ _ _ ->
          case runTI (infer body) mempty mkInference of
            Left err -> Left err
            Right ((_, ty, _), _) -> Right ty
        _ -> error "expected a value definition in test source"
