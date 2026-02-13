{-# LANGUAGE OverloadedStrings #-}

module Test.Change where

import qualified Data.Text as T
import Language.LSP.Protocol.Types (Position (Position))
import Server.Change
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Change"
    [ mkLSPMoveTests,
      applyLSPMoveTests
    ]

-- | Helper: build an LSPMove from 0-based Int coordinates.
mv :: Int -> Int -> Int -> Int -> T.Text -> LSPMove
mv sL sC eL eC = mkLSPMove (pos sL sC) (pos eL eC)

-- | Helper: build an LSP Position from 0-based Ints.
pos :: Int -> Int -> Position
pos l c = Position (fromIntegral l) (fromIntegral c)

-- | Helper: apply and compare using Int coordinates.
applyMv :: Int -> Int -> Int -> Int -> LSPMove -> Maybe (Position, Position)
applyMv oSL oSC oEL oEC = applyLSPMove (pos oSL oSC) (pos oEL oEC)

--------------------------------------------------------------------------------
-- mkLSPMove
--------------------------------------------------------------------------------

mkLSPMoveTests :: TestTree
mkLSPMoveTests =
  testGroup
    "mkLSPMove"
    [ testCase "single char insert" $ do
        let m = mv 0 5 0 5 "x"
        dL m @?= 0
        dC m @?= 1,
      testCase "single char delete" $ do
        let m = mv 0 5 0 6 ""
        dL m @?= 0
        dC m @?= -1,
      testCase "replace with longer text, same line" $ do
        let m = mv 0 5 0 7 "xyz"
        dL m @?= 0
        dC m @?= 1,
      testCase "insert newline" $ do
        let m = mv 0 5 0 5 "\n"
        dL m @?= 1
        dC m @?= -5, -- new end column is 0, was 5
      testCase "insert multi-line text" $ do
        let m = mv 1 3 1 3 "aa\nbb\ncc"
        dL m @?= 2
        dC m @?= -1, -- new end column is 2 ("cc"), was 3
      testCase "delete across lines" $ do
        let m = mv 1 5 3 2 ""
        dL m @?= -2
        dC m @?= 3 -- new end column is 5 (collapses to start), was 2
    ]

--------------------------------------------------------------------------------
-- applyLSPMove
--------------------------------------------------------------------------------

applyLSPMoveTests :: TestTree
applyLSPMoveTests =
  testGroup
    "applyLSPMove"
    [ beforeChangeTests,
      afterChangeTests,
      overlapTests
    ]

beforeChangeTests :: TestTree
beforeChangeTests =
  testGroup
    "before change (unchanged)"
    [ testCase "range on earlier line" $ do
        let m = mv 5 0 5 3 "xyz"
        applyMv 2 0 2 10 m @?= Just (pos 2 0, pos 2 10),
      testCase "range on same line, ends at change start" $ do
        let m = mv 3 10 3 15 "x"
        applyMv 3 5 3 10 m @?= Just (pos 3 5, pos 3 10),
      testCase "range on same line, ends before change start" $ do
        let m = mv 3 10 3 15 "x"
        applyMv 3 5 3 8 m @?= Just (pos 3 5, pos 3 8)
    ]

afterChangeTests :: TestTree
afterChangeTests =
  testGroup
    "after change (shifted)"
    [ testCase "range on later line, single-line insert" $ do
        let m = mv 3 0 3 0 "xyz"
        applyMv 5 2 5 8 m @?= Just (pos 5 2, pos 5 8),
      testCase "range on same line as change end" $ do
        -- replace (3,5)..(3,8) with "ab", dL=0 dC=-1
        let m = mv 3 5 3 8 "ab"
        applyMv 3 8 3 12 m @?= Just (pos 3 7, pos 3 11),
      testCase "range starts on change end line, ends on later line" $ do
        let m = mv 3 5 3 8 "ab"
        applyMv 3 8 4 2 m @?= Just (pos 3 7, pos 4 2),
      testCase "multi-line insert shifts later lines" $ do
        -- insert "a\nb\nc" at (2,0)..(2,0), dL=2 dC=1
        let m = mv 2 0 2 0 "a\nb\nc"
        applyMv 3 5 3 10 m @?= Just (pos 5 5, pos 5 10),
      testCase "multi-line delete shifts later lines up" $ do
        -- delete (2,0)..(4,0), dL=-2 dC=0
        let m = mv 2 0 4 0 ""
        applyMv 5 3 6 7 m @?= Just (pos 3 3, pos 4 7),
      testCase "range starts exactly at change end" $ do
        let m = mv 1 0 1 5 "hello world"
        applyMv 1 5 1 10 m @?= Just (pos 1 11, pos 1 16)
    ]

overlapTests :: TestTree
overlapTests =
  testGroup
    "overlapping (invalidated)"
    [ testCase "range contains change" $ do
        let m = mv 3 5 3 8 "x"
        applyMv 3 0 3 15 m @?= Nothing,
      testCase "change contains range" $ do
        let m = mv 3 0 3 15 "x"
        applyMv 3 5 3 8 m @?= Nothing,
      testCase "range start inside change" $ do
        let m = mv 3 5 3 10 "x"
        applyMv 3 7 3 15 m @?= Nothing,
      testCase "range end inside change" $ do
        let m = mv 3 5 3 10 "x"
        applyMv 3 0 3 7 m @?= Nothing,
      testCase "range start equals change start (inclusive)" $ do
        let m = mv 3 5 3 10 "x"
        applyMv 3 5 3 8 m @?= Nothing,
      testCase "multi-line overlap" $ do
        let m = mv 2 5 4 3 "x"
        applyMv 3 0 5 0 m @?= Nothing
    ]
