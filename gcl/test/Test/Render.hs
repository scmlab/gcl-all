{-# LANGUAGE OverloadedStrings #-}

module Test.Render where

import Data.Text (Text)
import qualified Data.Text as Text
import Pretty
  ( docToText,
    toText,
  )
import Prettyprinter (Pretty, pretty)
import Render.Element (inlinesToHtml)
import Render.Syntax.Typed (renderExprRZ, renderPOPredRZ)
import Server.Reduce (evalReduce)
import qualified Syntax.Abstract as A
import qualified Syntax.Common.Types as Common
import qualified Syntax.Concrete as C
import qualified Syntax.Concrete.Instances.ToAbstract as AT
import Syntax.Parser (Parser)
import qualified Syntax.Parser as Parser
import qualified Syntax.Typed.Types as T
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    testCase,
    (@?),
    (@?=),
  )
import Test.Util
  ( removeTrailingWhitespace,
  )

tests :: TestTree
tests =
  testGroup
    "Render"
    [ expression,
      redexAnnotations
    ]

--------------------------------------------------------------------------------

-- | Expression
expression :: TestTree
expression =
  testGroup
    "Expressions"
    [ testCase "1" $ run "a + b + c + d",
      testCase "2" $ run "a - (b - c) - d",
      testCase "3" $ run "a * (1 + 2) + 3",
      testCase "4" $ prettyae "a * ((1 + 2) + 3)" @?= "a * (1 + 2 + 3)",
      testCase "5" $ prettyae "a * (1 + (2 + 3))" @?= "a * (1 + 2 + 3)",
      testCase "6" $ prettyae "a * (b * c)" @?= "a * b * c",
      testCase "7" $ prettyae "a && b || c && d" @?= "((a && b) || c) && d",
      testCase "8" $ run "a || b || c",
      testCase "9" $ run "a => b => c",
      testCase "10" $ run "(a => b) => c",
      testCase "11" $ run "- a",
      testCase "12" $ run "f (- a)",
      testCase "13" $ prettyae "(- a) + b" @?= "- a + b",
      testCase "14" $ prettyae "- a + b - c" @?= "(- a + b) - c",
      testCase "15" $ run "a * (- a)",
      testCase "16" $ run "a => ~ b",
      testCase "17" $ run "a > b >= c",
      testCase "18" $ run "~ (a > b >= c)"
    ]
  where
    run :: Text -> Assertion
    run e = prettyae e @?= e
    prettyae = docToText . pretty . ae . ce

--------------------------------------------------------------------------------

redexAnnotations :: TestTree
redexAnnotations =
  testGroup
    "Redex annotations"
    [ testCase "a definition reference carries the Reduce2 path" $
        Text.isInfixOf
          "<span class=\"gcl-redex\" data-redex=\"0\">f</span>"
          (inlinesToHtml (renderExprRZ [(f, identity)] call))
          @? "the function Var should be marked, rather than its enclosing App",
      testCase "the server accepts the path emitted by the renderer" $
        evalReduce [(f, identity)] call [0] @?= Just (T.Var a intType Nothing),
      testCase "a Chain maps its leftmost expression to the last child" $
        Text.isInfixOf
          "<span class=\"gcl-redex\" data-redex=\"2\">f</span>"
          (inlinesToHtml (renderExprRZ [(f, T.Var x intType Nothing)] chain))
          @? "the marker should follow Chain's rightmost-first child order",
      testCase "a top-level implication keeps its nested App path" $
        Text.isInfixOf
          "<span class=\"gcl-redex\" data-redex=\"0,1\">p</span>"
          (inlinesToHtml (renderPOPredRZ [(p, truth)] implication))
          @? "the implication's left operand should retain path [0,1]"
    ]
  where
    f = Common.Name "f" Nothing
    x = Common.Name "x" Nothing
    a = Common.Name "a" Nothing
    b = Common.Name "b" Nothing
    p = Common.Name "p" Nothing
    q = Common.Name "q" Nothing
    intType = A.TBase A.TInt Nothing
    boolType = A.TBase A.TBool Nothing
    unaryType = A.mkArrowType intType intType
    binaryBoolType = A.mkArrowType boolType (A.mkArrowType boolType boolType)
    identity = T.Lam x intType (T.Var x intType Nothing) Nothing
    call = T.App (T.Var f unaryType Nothing) (T.Var a intType Nothing) Nothing
    chain =
      T.Chain
        ( T.More
            ( T.More
                (T.Pure (T.Var f intType Nothing))
                (Common.ChainOp (Common.LT Nothing))
                intType
                (T.Var a intType Nothing)
            )
            (Common.ChainOp (Common.LT Nothing))
            intType
            (T.Var b intType Nothing)
        )
    truth = T.Lit (A.Bol True) boolType Nothing
    implication =
      T.App
        (T.App (T.Op (Common.ArithOp (Common.ImpliesU Nothing)) binaryBoolType) (T.Var p boolType Nothing) Nothing)
        (T.Var q boolType Nothing)
        Nothing

--------------------------------------------------------------------------------

parserCompare :: (Pretty a) => Parser a -> Text -> Text -> Assertion
parserCompare parser actual expected =
  (removeTrailingWhitespace . toText . Parser.scanAndParse parser "<test>")
    actual
    @?= removeTrailingWhitespace expected

parserIso :: (Pretty a) => Parser a -> Text -> Assertion
parserIso parser raw = parserCompare parser raw raw

ce :: Text -> C.Expr
ce = unsafeFromRight . Parser.scanAndParse Parser.expression ""

ae :: C.Expr -> A.Expr
ae = AT.runAbstractTransform

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight (Left _) = error "Error raised from 'unsafeFromRight', probably caused by parsing failure."
