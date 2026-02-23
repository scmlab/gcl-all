{-# LANGUAGE OverloadedStrings #-}

module Test.Change where

import Control.Exception (ErrorCall, evaluate, try)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Server.Change
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Change"
    [ mkLSPMoveTests,
      mkLSPMovesTests,
      applyLSPMoveTests,
      applyLSPMovesToTokenTests
    ]

-- | Helper: build an LSPMove from 0-based Int coordinates.
mv :: Int -> Int -> Int -> Int -> T.Text -> LSPMove
mv sL sC eL eC = mkLSPMove (rng sL sC eL eC)

-- | Helper: build an LSP Position from 0-based Ints.
pos :: Int -> Int -> LSP.Position
pos l c = LSP.Position (fromIntegral l) (fromIntegral c)

-- | Helper: build an LSP Range from 0-based Int coordinates.
rng :: Int -> Int -> Int -> Int -> LSP.Range
rng sL sC eL eC = LSP.Range (pos sL sC) (pos eL eC)

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
-- mkLSPMoves
--------------------------------------------------------------------------------

mkLSPMovesTests :: TestTree
mkLSPMovesTests =
  testGroup
    "mkLSPMoves"
    [ testCase "converts incremental changes" $ do
        let moves = mkLSPMoves [partialChange 0 0 0 1 "x", partialChange 2 0 2 5 ""]
        length moves @?= 2
        dC (head moves) @?= 0   -- replace 1 char with 1 char
        dC (moves !! 1) @?= -5  -- delete 5 chars
    , testCase "errors on full document change" $ do
        result <- try (evaluate (head (mkLSPMoves [wholeChange "entire new content"]))) :: IO (Either ErrorCall LSPMove)
        case result of
          Left _  -> return ()
          Right _ -> assertFailure "expected error but got none"
    ]
  where
    partialChange sL sC eL eC text =
      LSP.TextDocumentContentChangeEvent
        (LSP.InL (LSP.TextDocumentContentChangePartial (LSP.Range (pos sL sC) (pos eL eC)) Nothing text))
    wholeChange text =
      LSP.TextDocumentContentChangeEvent
        (LSP.InR (LSP.TextDocumentContentChangeWholeDocument text))

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
        applyLSPMove (rng 2 0 2 10) m @?= Just (rng 2 0 2 10),
      testCase "range on same line, ends at change start" $ do
        let m = mv 3 10 3 15 "x"
        applyLSPMove (rng 3 5 3 10) m @?= Just (rng 3 5 3 10),
      testCase "range on same line, ends before change start" $ do
        let m = mv 3 10 3 15 "x"
        applyLSPMove (rng 3 5 3 8) m @?= Just (rng 3 5 3 8)
    ]

afterChangeTests :: TestTree
afterChangeTests =
  testGroup
    "after change (shifted)"
    [ testCase "range on later line, single-line insert" $ do
        let m = mv 3 0 3 0 "xyz"
        applyLSPMove (rng 5 2 5 8) m @?= Just (rng 5 2 5 8),
      testCase "range on same line as change end" $ do
        -- replace (3,5)..(3,8) with "ab", dL=0 dC=-1
        let m = mv 3 5 3 8 "ab"
        applyLSPMove (rng 3 8 3 12) m @?= Just (rng 3 7 3 11),
      testCase "range starts on change end line, ends on later line" $ do
        let m = mv 3 5 3 8 "ab"
        applyLSPMove (rng 3 8 4 2) m @?= Just (rng 3 7 4 2),
      testCase "multi-line insert shifts later lines" $ do
        -- insert "a\nb\nc" at (2,0)..(2,0), dL=2 dC=1
        let m = mv 2 0 2 0 "a\nb\nc"
        applyLSPMove (rng 3 5 3 10) m @?= Just (rng 5 5 5 10),
      testCase "multi-line delete shifts later lines up" $ do
        -- delete (2,0)..(4,0), dL=-2 dC=0
        let m = mv 2 0 4 0 ""
        applyLSPMove (rng 5 3 6 7) m @?= Just (rng 3 3 4 7),
      testCase "range starts exactly at change end" $ do
        let m = mv 1 0 1 5 "hello world"
        applyLSPMove (rng 1 5 1 10) m @?= Just (rng 1 11 1 16)
    ]

overlapTests :: TestTree
overlapTests =
  testGroup
    "overlapping (invalidated)"
    [ testCase "range contains change" $ do
        let m = mv 3 5 3 8 "x"
        applyLSPMove (rng 3 0 3 15) m @?= Nothing,
      testCase "change contains range" $ do
        let m = mv 3 0 3 15 "x"
        applyLSPMove (rng 3 5 3 8) m @?= Nothing,
      testCase "range start inside change" $ do
        let m = mv 3 5 3 10 "x"
        applyLSPMove (rng 3 7 3 15) m @?= Nothing,
      testCase "range end inside change" $ do
        let m = mv 3 5 3 10 "x"
        applyLSPMove (rng 3 0 3 7) m @?= Nothing,
      testCase "range start equals change start (inclusive)" $ do
        let m = mv 3 5 3 10 "x"
        applyLSPMove (rng 3 5 3 8) m @?= Nothing,
      testCase "multi-line overlap" $ do
        let m = mv 2 5 4 3 "x"
        applyLSPMove (rng 3 0 5 0) m @?= Nothing
    ]

--------------------------------------------------------------------------------
-- applyLSPMovesToToken
--------------------------------------------------------------------------------

-- | Helper: build a SemanticTokenAbsolute.
tok :: Int -> Int -> Int -> LSP.SemanticTokenAbsolute
tok l c len =
  LSP.SemanticTokenAbsolute
    (fromIntegral l)
    (fromIntegral c)
    (fromIntegral len)
    LSP.SemanticTokenTypes_Variable
    []

-- | Helper: check token position and length.
tokAt :: LSP.SemanticTokenAbsolute -> (Int, Int, Int)
tokAt (LSP.SemanticTokenAbsolute l c len _ _) = (fromIntegral l, fromIntegral c, fromIntegral len)

applyLSPMovesToTokenTests :: TestTree
applyLSPMovesToTokenTests =
  testGroup
    "applyLSPMovesToToken"
    [ testCase "no moves" $ do
        fmap tokAt (applyLSPMovesToToken [] (tok 3 5 4)) @?= Just (3, 5, 4),
      testCase "two sequential shifts accumulate" $ do
        -- first: insert "ab" at (3,0), dC=2
        -- second: insert "c" at (3,0), dC=1
        -- token at (3,5) -> (3,7) -> (3,8)
        let moves = [mv 3 0 3 0 "ab", mv 3 0 3 0 "c"]
        fmap tokAt (applyLSPMovesToToken moves (tok 3 5 4)) @?= Just (3, 8, 4),
      testCase "first move shifts, second sees new position" $ do
        -- first: insert "\n" at (3,0), shifts token from line 3 to line 4
        -- second: change on line 3 — token is now on line 4, no overlap
        let moves = [mv 3 0 3 0 "\n", mv 3 0 3 5 "x"]
        fmap tokAt (applyLSPMovesToToken moves (tok 3 5 4)) @?= Just (4, 5, 4),
      testCase "second move invalidates after first shifts" $ do
        -- first: insert "aaa" at (3,0), shifts token (3,5) -> (3,8)
        -- second: change (3,7)..(3,10) overlaps with token at (3,8..12)
        let moves = [mv 3 0 3 0 "aaa", mv 3 7 3 10 "x"]
        applyLSPMovesToToken moves (tok 3 5 4) @?= Nothing,
      testCase "invalidated at first move, short-circuits" $ do
        -- first move overlaps token, second is irrelevant
        let moves = [mv 3 4 3 8 "x", mv 0 0 0 0 "y"]
        applyLSPMovesToToken moves (tok 3 5 4) @?= Nothing,
      testCase "order matters: both survive but result depends on sequence" $ do
        -- token at (3,10) len 4 -> range (3,10)..(3,14)
        -- move A: delete (3,0)..(3,3), dL=0 dC=-3  (shrink before token)
        -- move B: insert "\n" at (3,5)..(3,5), dL=1 dC=-5  (newline before token)
        --
        -- correct (A then B):
        --   A: token (3,10)..(3,14) -> (3,7)..(3,11)
        --   B: change at (3,5), token start (3,7) >= (3,5), same line as eL=3
        --      dL=1 dC=-5: (3+1, 7-5) = (4,2) -> token (4,2)..(4,6)
        --
        -- wrong (B then A):
        --   B: change at (3,5), token start (3,10) >= (3,5)
        --      dL=1 dC=-5: (4,5) -> token (4,5)..(4,9)
        --   A: change (3,0)..(3,3), token on line 4, different from eL=3
        --      dL=0: (4,5)..(4,9) unchanged
        let moves = [mv 3 0 3 3 "", mv 3 5 3 5 "\n"]
        fmap tokAt (applyLSPMovesToToken moves (tok 3 10 4)) @?= Just (4, 2, 4)
    ]
