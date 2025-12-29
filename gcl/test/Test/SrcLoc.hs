{-# LANGUAGE OverloadedStrings #-}

module Test.SrcLoc where

import Data.List (sort)
import Data.Loc.Range
import GCL.Predicate (Origin (AtSkip))
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- Test Suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Source Location"
    [ posCoffMonotonicityTests,
      posCoffRangeTests,
      withinTests,
      sortingOriginsTests
    ]

--------------------------------------------------------------------------------
-- Test 1: posCoff maintains monotonicity (for IntervalMap)
--------------------------------------------------------------------------------

posCoffMonotonicityTests :: TestTree
posCoffMonotonicityTests =
  testGroup
    "posCoff monotonicity"
    [ testCase "same line, different cols" $ do
        let p1 = mkPos 1 10
            p2 = mkPos 1 20
        assertBool "col 10 < col 20" (posCoff p1 < posCoff p2),
      testCase "different lines" $ do
        let p1 = mkPos 1 100
            p2 = mkPos 2 1
        assertBool "line 1 col 100 < line 2 col 1" (posCoff p1 < posCoff p2),
      testCase "line difference dominates" $ do
        let p1 = mkPos 1 9999
            p2 = mkPos 2 1
        assertBool "line 1 col 9999 < line 2 col 1" (posCoff p1 < posCoff p2)
    ]

--------------------------------------------------------------------------------
-- Test 2: posCoff-based range comparisons (simulates IntervalMap lookup)
--------------------------------------------------------------------------------

posCoffRangeTests :: TestTree
posCoffRangeTests =
  testGroup
    "posCoff range comparison (end-exclusive)"
    [ testCase "position before range" $ do
        let pos = mkPos 1 5
            rng = mkRange (mkPos 1 10) (mkPos 1 20)  -- [10, 20)
        assertBool "pos before range" (posBeforeRange pos rng),
      testCase "position at range start (inclusive)" $ do
        let pos = mkPos 1 10
            rng = mkRange (mkPos 1 10) (mkPos 1 20)  -- [10, 20)
        assertBool "pos in range" (posInRange pos rng),
      testCase "position in range middle" $ do
        let pos = mkPos 1 15
            rng = mkRange (mkPos 1 10) (mkPos 1 20)  -- [10, 20)
        assertBool "pos in range" (posInRange pos rng),
      testCase "position just before range end" $ do
        let pos = mkPos 1 19
            rng = mkRange (mkPos 1 10) (mkPos 1 20)  -- [10, 20)
        assertBool "pos in range" (posInRange pos rng),
      testCase "position at range end (exclusive)" $ do
        let pos = mkPos 1 20
            rng = mkRange (mkPos 1 10) (mkPos 1 20)  -- [10, 20)
        assertBool "pos NOT in range (end-exclusive)" (not $ posInRange pos rng),
      testCase "position after range" $ do
        let pos = mkPos 1 25
            rng = mkRange (mkPos 1 10) (mkPos 1 20)  -- [10, 20)
        assertBool "pos after range" (posAfterRange pos rng)
    ]
  where
    -- Correct semantics for end-exclusive Range
    posInRange :: Pos -> Range -> Bool
    posInRange pos (Range start end) =
      posCoff start <= posCoff pos && posCoff pos < posCoff end  -- Note: < not <=

    posBeforeRange :: Pos -> Range -> Bool
    posBeforeRange pos (Range start _) = posCoff pos < posCoff start

    posAfterRange :: Pos -> Range -> Bool
    posAfterRange pos (Range _ end) = posCoff pos >= posCoff end  -- >= because end is exclusive

--------------------------------------------------------------------------------
-- Test 3: within function (from Data.Loc.Range)
--------------------------------------------------------------------------------

withinTests :: TestTree
withinTests =
  testGroup
    "within"
    [ testCase "identical ranges" $
        mkRange (mkPos 1 10) (mkPos 1 20)
          `within` mkRange (mkPos 1 10) (mkPos 1 20)
          @?= True,
      testCase "smaller range inside larger" $
        mkRange (mkPos 1 15) (mkPos 1 18)
          `within` mkRange (mkPos 1 10) (mkPos 1 20)
          @?= True,
      testCase "left boundary exceeds" $
        mkRange (mkPos 1 9) (mkPos 1 15)
          `within` mkRange (mkPos 1 10) (mkPos 1 20)
          @?= False,
      testCase "right boundary exceeds" $
        mkRange (mkPos 1 15) (mkPos 1 21)
          `within` mkRange (mkPos 1 10) (mkPos 1 20)
          @?= False,
      testCase "completely outside (before)" $
        mkRange (mkPos 1 1) (mkPos 1 5)
          `within` mkRange (mkPos 1 10) (mkPos 1 20)
          @?= False,
      testCase "completely outside (after)" $
        mkRange (mkPos 1 25) (mkPos 1 30)
          `within` mkRange (mkPos 1 10) (mkPos 1 20)
          @?= False
    ]

--------------------------------------------------------------------------------
-- Test 4: sorting Origins (GCL-specific)
--------------------------------------------------------------------------------

sortingOriginsTests :: TestTree
sortingOriginsTests =
  testGroup
    "sorting Origins"
    [ testCase "basic sorting" $
        sort [mk 10 20, mk 20 30, mk 11 19, mk 21 29]
          @?= [mk 11 19, mk 10 20, mk 21 29, mk 20 30],
      testCase "identical ranges" $
        sort [mk 80 184, mk 80 184, mk 92 102, mk 92 102]
          @?= [mk 92 102, mk 92 102, mk 80 184, mk 80 184],
      testCase "overlapping ranges" $
        sort [mk 10 20, mk 15 25, mk 20 30]
          @?= [mk 10 20, mk 15 25, mk 20 30]
    ]
  where
    mk :: Int -> Int -> Origin
    mk startCol endCol = AtSkip (Just (mkRange (mkPos 1 startCol) (mkPos 1 endCol)))
