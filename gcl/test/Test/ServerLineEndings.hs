{-# LANGUAGE OverloadedStrings #-}

module Test.ServerLineEndings (tests) where

import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Predicate (Spec (..))
import GCL.Range (mkPos, mkRange)
import Server.FileState (FileState (..))
import Server.Load (applyEdits, loadAndDig, parseAndDig, withSourceLineEndings)
import Server.Refine (refineAndDig)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Server line endings"
    [ pipelineTests "LF" "\n",
      pipelineTests "CRLF" "\r\n",
      testCase "a source without newlines defaults to LF" $ do
        (dig, _) <- expectRight (parseAndDig "test.gcl" "?")
        case dig of
          Just (edits, newSource) -> do
            map snd edits @?= ["[!\n\n!]"]
            newSource @?= "[!\n\n!]"
          Nothing -> assertFailure "expected a hole-digging edit",
      testCase "mixed source line endings use the first newline" $ do
        withSourceLineEndings "first\nsecond\r\n" "a\r\nb\n" @?= "a\nb\n"
        withSourceLineEndings "first\r\nsecond\n" "a\r\nb\n" @?= "a\r\nb\r\n"
    ]

pipelineTests :: String -> Text -> TestTree
pipelineTests label eol =
  testGroup
    label
    [ testCase "Reload uses source line endings in edits and expected content" $ do
        let source = linesWith ["  var x: Int", "  ?", "  { x > 0 }"]
            replacement = linesWith ["[!", "  ", "  !]"]
            expectedSource = linesWith ["  var x: Int", "  [!", "  ", "  !]", "  { x > 0 }"]
        (dig, result) <- expectRight (loadAndDig "test.gcl" source)
        case dig of
          Just (edits, newSource) -> do
            edits @?= [(mkRange (mkPos 2 3) (mkPos 2 4), replacement)]
            newSource @?= expectedSource
            applyEdits source edits @?= newSource
          Nothing -> assertFailure "expected a hole-digging edit"
        fs <- expectRight result
        case fsSpecifications fs of
          [spec] -> specRange spec @?= mkRange (mkPos 2 3) (mkPos 4 5)
          _ -> assertFailure "expected one specification",
      testCase "Reload preserves source line endings across multiple holes" $ do
        let source = linesWith ["var x: Int", "?", "?", "{ x > 0 }"]
            replacement = linesWith ["[!", "", "!]"]
            expectedSource = linesWith ["var x: Int", "[!", "", "!]", "[!", "", "!]", "{ x > 0 }"]
        (dig, result) <- expectRight (loadAndDig "test.gcl" source)
        case dig of
          Just (edits, newSource) -> do
            map snd edits @?= [replacement, replacement]
            newSource @?= expectedSource
            applyEdits source edits @?= newSource
          Nothing -> assertFailure "expected hole-digging edits"
        fs <- expectRight result
        length (fsSpecifications fs) @?= 2,
      testCase "Refine uses source line endings when digging a nested hole" $ do
        let source = linesWith ["var x: Int", "[!", "?", "!]", "{ x > 0 }"]
            expectedImpl = linesWith ["", "[!", "", "!]", ""]
            expectedSource = linesWith ["var x: Int", "", "[!", "", "!]", "", "{ x > 0 }"]
        (_, loaded) <- expectRight (loadAndDig "test.gcl" source)
        fs <- expectRight loaded
        case fsSpecifications fs of
          [spec] -> do
            (impl, _, result) <- expectRight (refineAndDig "test.gcl" (fsIdCount fs) spec source)
            impl @?= expectedImpl
            applyEdits source [(specRange spec, impl)] @?= expectedSource
            fragment <- expectRight result
            length (fsSpecifications fragment) @?= 1
          _ -> assertFailure "expected one specification"
    ]
  where
    linesWith = Text.intercalate eol

expectRight :: (Show e) => Either e a -> IO a
expectRight (Right value) = pure value
expectRight (Left err) = assertFailure (show err) >> fail "expected Right"
