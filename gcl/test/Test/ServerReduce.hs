{-# LANGUAGE OverloadedStrings #-}

module Test.ServerReduce (tests) where

import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GCL.Predicate (PO (..), Spec (..))
import qualified GCL.Predicate as GCL
import GCL.Range (mkPos, mkRange)
import qualified Language.LSP.Protocol.Types as LSP
import Server.FileState (FileState (..), emptyFileStateWithErrors)
import qualified Server.Handler.GCL.Reduce as Handler
import Server.Load (loadAndDig)
import Server.Move (applyMovesToFileState, mkLSPMove)
import Server.Reduce (reduceFileState)
import Server.Reduce.Types (ReduceTarget (..))
import Server.Refine (sweepFragment, updateHoleExprs)
import Server.ToClient (toClientFileStateJSON)
import Syntax.Abstract.Operator (tBool, tInt)
import Syntax.Common.Types (Name (..))
import qualified Syntax.Typed.Operator as Op
import qualified Syntax.Typed.Types as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Server reduction"
    [ testCase "old PO requests remain accepted" $
        JSON.eitherDecode "{\"filePath\":\"test.gcl\",\"po\":0,\"redex\":[]}"
          @?= Right (Handler.ReduceParams "test.gcl" (ProofObligation 0) []),
      testCase "typed targets round-trip through the request protocol" $
        mapM_
          (\target -> let request = Handler.ReduceParams "test.gcl" target [1, 0] in JSON.fromJSON (JSON.toJSON request) @?= JSON.Success request)
          [ProofObligation 0, SpecificationPrecondition 41, SpecificationPostcondition 41],
      testCase "precondition clicks change only the selected display expression" $ do
        let result = reduced (SpecificationPrecondition 41) [] initial
            spec = onlySpec result
        specReducedPreCond spec @?= Op.gt (Op.number 1) (Op.number 0)
        specPreCond spec @?= specPreCond originalSpec
        specPostCond spec @?= specPostCond originalSpec
        specReducedPostCond spec @?= specReducedPostCond originalSpec
        fsProofObligations result @?= fsProofObligations initial,
      testCase "postcondition clicks leave precondition and original contract intact" $ do
        let before = reduced (SpecificationPrecondition 41) [] initial
            after = reduced (SpecificationPostcondition 41) [] before
            spec = onlySpec after
        specReducedPreCond spec @?= specReducedPreCond (onlySpec before)
        specReducedPostCond spec @?= Op.lt (Op.number 2) (Op.number 10)
        specPreCond spec @?= specPreCond originalSpec
        specPostCond spec @?= specPostCond originalSpec,
      testCase "specification targets are stable IDs rather than list indices" $ do
        let other = originalSpec {specID = 7}
            before = initial {fsSpecifications = [other, originalSpec]}
            after = reduced (SpecificationPrecondition 41) [] before
        map specID (fsSpecifications after) @?= [7, 41]
        case fsSpecifications after of
          [unchanged, selected] -> do
            unchanged @?= other
            specReducedPreCond selected @?= Op.gt (Op.number 1) (Op.number 0)
          _ -> assertFailure "expected two specifications",
      testCase "consecutive definition clicks use the latest display expression" $ do
        let f = Name "f" Nothing
            g = Name "g" Nothing
            start = Op.nameVar f tBool
            spec = originalSpec {specPreCond = start, specReducedPreCond = start}
            before = initial {fsSpecifications = [spec], fsDefinitions = [(f, Op.nameVar g tBool), (g, Op.true)]}
            once = reduced (SpecificationPrecondition 41) [] before
            twice = reduced (SpecificationPrecondition 41) [] once
        specReducedPreCond (onlySpec once) @?= Op.nameVar g tBool
        specReducedPreCond (onlySpec twice) @?= Op.true
        specPreCond (onlySpec twice) @?= start,
      testCase "PO clicks still update only poReducedPred" $ do
        let before = initial {fsProofObligations = [originalPO]}
            after = reduced (ProofObligation 0) [] before
            po = case fsProofObligations after of
              [value] -> value
              _ -> error "expected one proof obligation"
        poReducedPred po @?= Op.gt (Op.number 1) (Op.number 0)
        poPred po @?= poPred originalPO
        poSMTResult po @?= poSMTResult originalPO
        fsSpecifications after @?= fsSpecifications before,
      testCase "stale IDs, invalid PO indices and invalid paths are rejected" $
        mapM_
          (\(target, path) -> assertBool "expected a reduction error" (isLeft (reduceFileState target path initial)))
          [(SpecificationPrecondition 0, []), (SpecificationPostcondition 999, []), (ProofObligation 0, []), (ProofObligation (-1), []), (SpecificationPrecondition 41, [999])],
      testCase "the panel receives HTML targets while inlay-hint text stays unchanged" $ do
        let (pre, post, preHtml, postHtml) = payload initial
            (pre', post', preHtml', postHtml') = payload (reduced (SpecificationPrecondition 41) [] initial)
        pre' @?= pre
        post' @?= post
        postHtml' @?= postHtml
        assertBool "display HTML must change" (preHtml /= preHtml')
        assertBool "plain conditions must remain plain text" (not (Text.isInfixOf "<span" pre || Text.isInfixOf "<span" post))
        targetFromHtml preHtml @?= SpecificationPrecondition 41
        targetFromHtml postHtml @?= SpecificationPostcondition 41
        assertBool "root substitution must be clickable" (Text.isInfixOf "data-redex=\"\"" preHtml),
      testCase "Refine uses original conditions after either side was reduced" $ do
        let after = reduced (SpecificationPostcondition 41) [] (reduced (SpecificationPrecondition 41) [] initial)
        sweepFragment 100 (onlySpec after) [T.Skip Nothing]
          @?= sweepFragment 100 originalSpec [T.Skip Nothing],
      testCase "hole refinement resets display conditions to the updated originals" $ do
        let range = specRange originalSpec
            sourceHole = GCL.Hole 0 tBool range mempty (const Nothing)
            expressionHole = T.EHole (T.Hole "" 0 tBool range mempty)
            spec = originalSpec {specPreCond = expressionHole, specPostCond = expressionHole, specReducedPreCond = Op.false, specReducedPostCond = Op.false}
            after = updateHoleExprs sourceHole Op.true [] initial {fsSpecifications = [spec]}
            result = onlySpec after
        specPreCond result @?= Op.true
        specPostCond result @?= Op.true
        specReducedPreCond result @?= Op.true
        specReducedPostCond result @?= Op.true,
      testCase "moving source ranges preserves specification IDs and display reductions" $ do
        let before = reduced (SpecificationPrecondition 41) [] initial
            insertion = mkLSPMove (LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)) "\n"
            after = applyMovesToFileState [insertion] before
        specID (onlySpec after) @?= 41
        specReducedPreCond (onlySpec after) @?= specReducedPreCond (onlySpec before)
        assertBool "the range must move" (specRange (onlySpec after) /= specRange originalSpec),
      testCase "loading specifications initializes fresh display expressions" $ do
        let load = case loadAndDig "test.gcl" "var x : Int\n{ x = 0 }\n[!\n  skip\n!]\nx := x + 1\n{ x = 1 }\n" of
              Right (_, Right fs) -> fs
              _ -> error "test program could not be loaded"
            spec = onlySpec load
            clicked = reduced (SpecificationPostcondition (specID spec)) [] load
        specReducedPreCond spec @?= specPreCond spec
        specReducedPostCond spec @?= specPostCond spec
        assertBool "clicking must change the display" (specReducedPostCond (onlySpec clicked) /= specReducedPostCond spec)
        specReducedPostCond (onlySpec load) @?= specPostCond spec
    ]

originalSpec :: Spec
originalSpec =
  Specification
    { specID = 41,
      specPreCond = pre,
      specPostCond = post,
      specReducedPreCond = pre,
      specReducedPostCond = post,
      specRange = mkRange (mkPos 3 1) (mkPos 5 3),
      specTypeEnv = mempty
    }
  where
    x = Name "x" Nothing
    pre = T.Subst (Op.gt (Op.nameVar x tInt) (Op.number 0)) [(x, Op.number 1)]
    post = T.Subst (Op.lt (Op.nameVar x tInt) (Op.number 10)) [(x, Op.number 2)]

originalPO :: PO
originalPO = PO predicate predicate "original proof text" False "existing result" Nothing (GCL.AtSpec Nothing)
  where
    predicate = specPreCond originalSpec

initial :: FileState
initial = (emptyFileStateWithErrors []) {fsSpecifications = [originalSpec]}

onlySpec :: FileState -> Spec
onlySpec fs = case fsSpecifications fs of
  [spec] -> spec
  _ -> error "expected one specification"

reduced :: ReduceTarget -> [Int] -> FileState -> FileState
reduced target path fs = either (error . show) id (reduceFileState target path fs)

payload :: FileState -> (Text, Text, Text, Text)
payload fs = case JSON.parseEither parser (toClientFileStateJSON "test.gcl" fs) of
  Right [value] -> value
  result -> error (show result)
  where
    parser = JSON.withObject "FileState" $ \obj -> do
      specs <- obj .: "specs"
      mapM (JSON.withObject "Spec" $ \spec -> (,,,) <$> spec .: "preCondition" <*> spec .: "postCondition" <*> spec .: "reducedPreCond" <*> spec .: "reducedPostCond") specs

targetFromHtml :: Text -> ReduceTarget
targetFromHtml html =
  let marker = "data-target='"
      target = Text.takeWhile (/= '\'') (Text.drop (Text.length marker) (snd (Text.breakOn marker html)))
   in either error id (JSON.eitherDecodeStrict (Text.encodeUtf8 target))
