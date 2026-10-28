{-# LANGUAGE OverloadedStrings #-}

module Test.Config (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Either (isLeft)
import Server.Config
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Server configuration"
    [ testCase "empty settings use the defaults" $
        parseConfig defaultConfig (object []) @?= Right (Config 2000 100),
      testCase "both uppercase SMT settings can be configured" $
        parseConfig defaultConfig (object ["SMTTimeoutMs" .= (350 :: Int), "SMTReductionLimit" .= (50 :: Int)])
          @?= Right (Config 350 50),
      testCase "a partial update preserves the other limit" $
        parseConfig (Config 350 50) (object ["SMTReductionLimit" .= (10 :: Int)])
          @?= Right (Config 350 10),
      testCase "explicit defaults reset previous settings" $
        parseConfig (Config 350 50) (object ["SMTTimeoutMs" .= (2000 :: Int), "SMTReductionLimit" .= (100 :: Int)])
          @?= Right defaultConfig,
      testCase "unrelated extension settings are ignored" $
        parseConfig defaultConfig (object ["gclPath" .= String "/tmp/gcl", "trace" .= object ["server" .= String "verbose"]])
          @?= Right defaultConfig,
      testCase "both limits reject invalid values" $
        mapM_
          (\key -> mapM_ (\value -> assertBool (show (key, value)) (isLeft (parseConfig defaultConfig (object [key .= value])))) invalidValues)
          ["SMTTimeoutMs", "SMTReductionLimit"],
      testCase "non-object settings are rejected" $
        assertBool "expected a settings object" (isLeft (parseConfig defaultConfig (String "invalid")))
    ]
  where
    invalidValues = [Number 0, Number (-1), Number 1.5, Number 2147483648, String "100", Bool True]
