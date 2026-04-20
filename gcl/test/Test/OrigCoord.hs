{-# LANGUAGE OverloadedStrings #-}

module Test.OrigCoord where

import GCL.Range (Pos, Range, mkPos, mkRange)
import Server.OrigCoord
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "OrigCoord"
    [ prepareEditsTests,
      convertStartTests,
      convertEndTests,
      convertRangeTests,
      multiEditTests,
      manualTest
    ]

-- Helpers
p :: Int -> Int -> Pos
p = mkPos

r :: Int -> Int -> Int -> Int -> Range
r sl sc el ec = mkRange (p sl sc) (p el ec)

--------------------------------------------------------------------------------
-- prepareEdits
--------------------------------------------------------------------------------

prepareEditsTests :: TestTree
prepareEditsTests =
  testGroup
    "prepareEdits"
    [ testCase "single edit: abc -> pqrst (same line)" $ do
        -- Original: 12abc3456   (positions on line 1, 1-based)
        -- New:      12pqrst3456
        -- Edit range in original: (1,3)-(1,6)  i.e. "abc"
        -- New text: "pqrst"
        -- In new coords: start (1,3), end (1,8)
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        ers
          @?= [ EditRegion
                  { erOrigStart = p 1 3,
                    erOrigEnd = p 1 6,
                    erNewStart = p 1 3,
                    erNewEnd = p 1 8
                  }
              ],
      testCase "two edits: R1 and R2 on same line" $ do
        -- Original: AARRBBSSCC  (line 1)
        --           AA = cols 1-2, RR = cols 3-4, BB = cols 5-6, SS = cols 7-8, CC = cols 9-10
        -- Edit 1: replace cols 3-5 (RR) with "XXX"  -> delta +1
        -- Edit 2: replace cols 7-9 (SS) with "Y"    -> delta -1
        -- New:     AAXXXBBYCCC (conceptually)
        let ers =
              prepareEdits
                [ Edit (r 1 3 1 5) "XXX",
                  Edit (r 1 7 1 9) "Y"
                ]
        length ers @?= 2
        -- First edit: no prior delta, so newStart = origStart
        erNewStart (head ers) @?= p 1 3
        erNewEnd (head ers) @?= p 1 6 -- 3 + 3 = 6
        -- Second edit: prior delta is +1 col, so newStart = 7+1 = 8
        erNewStart (ers !! 1) @?= p 1 8
        erNewEnd (ers !! 1) @?= p 1 9, -- 8 + 1 = 9
      testCase "multi-line edit: insert newlines" $ do
        -- Original line 1: "hello world" (cols 1-11)
        -- Edit: replace (1,6)-(1,7) with "\nX\n"  (replace " " with newline-X-newline)
        -- New text has 2 newlines, so new end is line 3, col 1
        let ers = prepareEdits [Edit (r 1 6 1 7) "\nX\n"]
        erNewStart (head ers) @?= p 1 6
        erNewEnd (head ers) @?= p 3 1
    ]

--------------------------------------------------------------------------------
-- convertPos SideStart
--------------------------------------------------------------------------------

convertStartTests :: TestTree
convertStartTests =
  testGroup
    "convertPos SideStart"
    [ testCase "before edit: unchanged" $ do
        -- Original: 12abc3456
        -- New:      12pqrst3456
        -- Edit: (1,3)-(1,6) -> "pqrst", new region (1,3)-(1,8)
        -- Position (1,2) is in the "12" part, before the edit. No change.
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertPos SideStart ers (p 1 2) @?= p 1 2,
      testCase "after edit: shifted back" $ do
        -- Position (1,9) in new coords is the "4" in "12pqrst3456"
        -- The edit added 2 columns (5-3=2), so original position is 9-2 = 7
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertPos SideStart ers (p 1 9) @?= p 1 7,
      testCase "inside edit: expand to original start" $ do
        -- Position (1,5) is inside "pqrst" in new coords
        -- Start expands to the original edit start: (1,3)
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertPos SideStart ers (p 1 5) @?= p 1 3,
      testCase "at edit start boundary: expand to original start" $ do
        -- Position (1,3) is the first char of "pqrst"
        -- For SideStart, pos == newStart means inside, expand to origStart
        -- (Actually pos < newStart is false, so it goes to the "inside" branch)
        -- This is correct: start at beginning of edit = expand
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertPos SideStart ers (p 1 3) @?= p 1 3 -- happens to be same value
    ]

--------------------------------------------------------------------------------
-- convertPos SideEnd
--------------------------------------------------------------------------------

convertEndTests :: TestTree
convertEndTests =
  testGroup
    "convertPos SideEnd"
    [ testCase "before edit: unchanged" $ do
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertPos SideEnd ers (p 1 2) @?= p 1 2,
      testCase "after edit: shifted back" $ do
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertPos SideEnd ers (p 1 9) @?= p 1 7,
      testCase "at edit new-start (end-exclusive doesn't touch edit content): no change" $ do
        -- End-exclusive position (1,3) = "p" position in new coords.
        -- But end-exclusive means the range stops BEFORE "p", so it doesn't
        -- actually include any edited content. Should map as if before the edit.
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertPos SideEnd ers (p 1 3) @?= p 1 3,
      testCase "inside edit (after first char): expand to original end" $ do
        -- End-exclusive position (1,5) is inside "pqrst" (past "p").
        -- This means the range includes some edited content.
        -- Expand to original edit end: (1,6)
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertPos SideEnd ers (p 1 5) @?= p 1 6,
      testCase "at edit new-end: shifted (not inside edit)" $ do
        -- End-exclusive position (1,8) = newEnd of the edit.
        -- Range covers the entire edit content. This position equals newEnd,
        -- so it's NOT inside the edit (the loop condition pos < newEnd is false).
        -- It gets shifted: 8 - 2 = 6, which equals origEnd.
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertPos SideEnd ers (p 1 8) @?= p 1 6
    ]

--------------------------------------------------------------------------------
-- convertRange: full range conversion
--------------------------------------------------------------------------------

convertRangeTests :: TestTree
convertRangeTests =
  testGroup
    "convertRange"
    [ testCase "range entirely before edit: unchanged" $ do
        -- Range (1,1)-(1,3) is "12" in both old and new
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertRange ers (r 1 1 1 3) @?= r 1 1 1 3,
      testCase "range entirely after edit: shifted" $ do
        -- Range (1,8)-(1,10) in new coords -> (1,6)-(1,8) in orig
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertRange ers (r 1 8 1 10) @?= r 1 6 1 8,
      testCase "range spanning from before into edit: start unchanged, end expanded" $ do
        -- Original: 12abc3456
        -- New:      12pqrst3456
        -- Range (1,1)-(1,5) in new coords = "12pq" (end at 'r')
        -- Start (1,1): before edit -> (1,1)
        -- End (1,5): inside edit -> expand to origEnd (1,6)
        -- Result: (1,1)-(1,6)
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertRange ers (r 1 1 1 5) @?= r 1 1 1 6,
      testCase "range spanning from edit into after: start expanded, end shifted" $ do
        -- Range (1,5)-(1,10) in new coords
        -- Start (1,5): inside edit -> expand to origStart (1,3)
        -- End (1,10): after edit -> shift by -2 -> (1,8)
        -- Result: (1,3)-(1,8)
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertRange ers (r 1 5 1 10) @?= r 1 3 1 8,
      testCase "range entirely inside edit: expanded to full original edit range" $ do
        -- Range (1,4)-(1,6) in new coords = "qrs" (inside "pqrst")
        -- Start: inside edit -> origStart (1,3)
        -- End (1,6): inside edit -> origEnd (1,6)
        -- Result: (1,3)-(1,6) = the entire original edit range
        let ers = prepareEdits [Edit (r 1 3 1 6) "pqrst"]
        convertRange ers (r 1 4 1 6) @?= r 1 3 1 6
    ]

--------------------------------------------------------------------------------
-- Multiple edits
--------------------------------------------------------------------------------

multiEditTests :: TestTree
multiEditTests =
  testGroup
    "multiple edits"
    [ testCase "two edits: error between them shifts correctly" $ do
        -- Original: AA_RR_BB_SS_CC  (conceptual, all on line 1)
        --   A = cols 1-2, R = cols 3-4, B = cols 5-6, S = cols 7-8, C = cols 9-10
        -- Edit 1: (1,3)-(1,5) "RR" -> "XXX" (+1 col)
        -- Edit 2: (1,7)-(1,9) "SS" -> "Y"   (-1 col)
        -- New:    AA_XXX_BB_Y_CC
        -- After edit 1: +1 col delta
        -- After edit 2: +1 + (-1) = 0 col delta
        --
        -- Error in BB region, new coords (1,7)-(1,8) (shifted +1 by edit 1)
        -- Should map back to original (1,6)-(1,7) (undo the +1 from edit 1)
        let ers =
              prepareEdits
                [ Edit (r 1 3 1 5) "XXX",
                  Edit (r 1 7 1 9) "Y"
                ]
        -- Position in B region: between edit 1 and edit 2 in new coords
        -- In new coords, B is at cols 7-8 (original 5-6, shifted +1 by edit1's +1 delta)
        -- Wait, let me recalculate. Original: col 5 "B". Edit 1 adds 1 col.
        -- So new coord of col 5 = col 6. New coord of col 6 = col 7.
        -- Edit 2 in new coords starts at col 8+1=9 (orig 7, + delta 1+1... hmm)
        -- Let me just check via prepareEdits output
        erNewStart (head ers) @?= p 1 3
        erNewEnd (head ers) @?= p 1 6
        erNewStart (ers !! 1) @?= p 1 8
        erNewEnd (ers !! 1) @?= p 1 9

        -- Error at new coords (1,7)-(1,8): this is in the B region
        -- between the two edits. Edit 1 shifted by +1, so reverse is -1.
        convertRange ers (r 1 7 1 8) @?= r 1 6 1 7,
      testCase "two edits: error spanning both edits" $ do
        -- Same setup as above.
        -- Error spanning from inside edit 1 to inside edit 2 in new coords:
        -- new (1,4)-(1,9) -> start inside edit1 -> origStart1 (1,3)
        --                    end at newEnd2 (1,9) -> origEnd2 (1,9)
        -- Wait: end (1,9) == newEnd of edit 2, so it's shifted, not expanded.
        -- Shift: undo total delta. After both edits the total delta is 0,
        -- so (1,9) -> (1,9). And origEnd2 is also (1,9). Consistent.
        let ers =
              prepareEdits
                [ Edit (r 1 3 1 5) "XXX",
                  Edit (r 1 7 1 9) "Y"
                ]
        convertRange ers (r 1 4 1 9) @?= r 1 3 1 9,
      testCase "multi-line edit: error after newline insertion" $ do
        -- Original (all on line 1): "AB CD"
        -- Edit: replace " " at (1,3)-(1,4) with "\n"
        -- New:
        --   line 1: "AB"
        --   line 2: "CD"
        -- Error at new (2,1)-(2,3) = "CD" on line 2
        -- In original coords that's (1,4)-(1,6)
        let ers = prepareEdits [Edit (r 1 3 1 4) "\n"]
        -- erNewEnd should be (2,1)
        erNewEnd (head ers) @?= p 2 1
        -- The error on line 2 should map back to line 1
        convertRange ers (r 2 1 2 3) @?= r 1 4 1 6
    ]

--------------------------------------------------------------------------------
-- Manual test from orig_coordinate_3.md
--------------------------------------------------------------------------------

manualTest :: TestTree
manualTest =
  testGroup
    "manual: hole/spec edits"
    [ testCase "prepareEdits: new coordinates are correct" $ do
        -- Original file (1-based):
        --   line 1: "var i : Int"
        --   line 2: ""
        --   line 3: "i := ? + 3"         ? at col 6
        --   line 4: "?"                   ? at col 1
        --   line 5: "i := ? + 5 + True"  ? at col 6
        --   line 6: "?"                   ? at col 1
        --   line 7: "i := ? + 7"         ? at col 6
        --   line 8: ""
        --   line 9: "{True}"
        --
        -- Edits (all in original coordinates, ordered):
        --   1. (3,6)-(3,7) -> "{! !}"      hole on line 3
        --   2. (4,1)-(4,2) -> "[!\n\n!]"   spec on line 4
        --   3. (5,6)-(5,7) -> "{! !}"      hole on line 5
        --   4. (6,1)-(6,2) -> "[!\n\n!]"   spec on line 6
        --   5. (7,6)-(7,7) -> "{! !}"      hole on line 7
        --
        -- New file:
        --   line  1: "var i : Int"
        --   line  2: ""
        --   line  3: "i := {! !} + 3"
        --   line  4: "[!"
        --   line  5: ""
        --   line  6: "!]"
        --   line  7: "i := {! !} + 5 + True"
        --   line  8: "[!"
        --   line  9: ""
        --   line 10: "!]"
        --   line 11: "i := {! !} + 7"
        --   line 12: ""
        --   line 13: "{True}"
        let ers = prepareEdits edits

        -- Edit 1: (3,6)-(3,7) -> "{! !}", first edit so newStart = origStart
        erNewStart (ers !! 0) @?= p 3 6
        erNewEnd (ers !! 0) @?= p 3 11 -- 6 + 5 = 11

        -- Edit 2: (4,1)-(4,2) -> "[!\n\n!]"
        -- origEnd of edit 1 is (3,7), newEnd is (3,11), delta +4 cols on line 3
        -- origStart of edit 2 is (4,1) — different line from edit 1's end, so no col shift
        -- newStart = (4+0, 1) = (4,1)... wait, line delta: newEnd.line - origEnd.line = 3-3 = 0
        -- So newStart line = 4 + 0 = 4, col = 1 (different line from refLine 3)
        erNewStart (ers !! 1) @?= p 4 1
        -- "[!\n\n!]" from (4,1): line 4 "[!", line 5 "", line 6 "!]"
        erNewEnd (ers !! 1) @?= p 6 3 -- 3 lines, last line "!]" has len 2, col = 2+1 = 3

        -- Edit 3: (5,6)-(5,7) -> "{! !}"
        -- Previous edit ended at orig (4,2), new (6,3), so delta is +2 lines
        -- origStart (5,6): different line from origEnd (4,2)
        -- newStart line = 6 + (5-4) = 7, col = 6
        erNewStart (ers !! 2) @?= p 7 6
        erNewEnd (ers !! 2) @?= p 7 11

        -- Edit 4: (6,1)-(6,2) -> "[!\n\n!]"
        -- Previous edit ended at orig (5,7), new (7,11)
        -- origStart (6,1): different line from (5,7)
        -- newStart line = 7 + (6-5) = 8, col = 1
        erNewStart (ers !! 3) @?= p 8 1
        erNewEnd (ers !! 3) @?= p 10 3

        -- Edit 5: (7,6)-(7,7) -> "{! !}"
        -- Previous edit ended at orig (6,2), new (10,3)
        -- origStart (7,6): different line from (6,2)
        -- newStart line = 10 + (7-6) = 11, col = 6
        erNewStart (ers !! 4) @?= p 11 6
        erNewEnd (ers !! 4) @?= p 11 11,
      testCase "error 7:6-7:22 -> 5:6-5:18 (+ 5 + True has type error)" $ do
        -- In the new file, line 7 is: "i := {! !} + 5 + True"
        --   col 6: start of "{! !}" (inside edit 3)
        --   col 22: end-exclusive after "True"
        --        i(1) (2):=(3,4) (5){(6)!(7) (8)!(9)}(10) (11)+(12) (13)5(14) (15)+(16) (17)T(18)r(19)u(20)e(21) -> 22
        --
        -- Start 7:6 is inside edit 3's new region (7,6)-(7,11)
        --   -> expand to edit 3's origStart = (5,6)
        --
        -- End 7:22 is after edit 3's new region (7,11)
        --   In original, edit 3 ended at (5,7), new ended at (7,11)
        --   reverseShift: same line as newEnd (7), so
        --     orig line = 5, orig col = 7 + (22-11) = 18
        --   -> (5,18)
        --
        -- Original line 5: "i := ? + 5 + True"
        --   col 18 is end-exclusive after "True": i(1) (2):=(3,4) (5)?(6) (7)+(8) (9)5(10) (11)+(12) (13)T(14)r(15)u(16)e(17) -> 18
        --   Yes! (5,6)-(5,18) covers "? + 5 + True" which makes sense as the error range.
        let ers = prepareEdits edits
        convertRange ers (r 7 6 7 22) @?= r 5 6 5 18,
      testCase "error on unchanged region after all edits" $ do
        -- {True} is on line 13 in new file, line 9 in original
        -- In new coords: (13,1)-(13,7)
        -- After all edits, line delta is +4 (edits 2,4 each add 2 lines)
        -- Col unchanged (different line from any edit end)
        -- Should map back to (9,1)-(9,7)
        let ers = prepareEdits edits
        convertRange ers (r 13 1 13 7) @?= r 9 1 9 7,
      testCase "error spanning from unchanged into edit region" $ do
        -- In new file line 3: "i := {! !} + 3"
        -- Error (3,1)-(3,8): "i := {!" — end is inside edit 1's new region (3,6)-(3,11)
        -- Start: before edit -> unchanged (3,1)
        -- End (3,8): inside edit 1 (past newStart) -> expand to origEnd (3,7)
        -- Result: (3,1)-(3,7) = "i := ?" in original
        let ers = prepareEdits edits
        convertRange ers (r 3 1 3 8) @?= r 3 1 3 7,
      testCase "error end at edit newStart (end-exclusive, no expansion)" $ do
        -- Error (3,1)-(3,6): "i := " — end at newStart of edit 1
        -- End-exclusive at (3,6) means range stops before the edit content
        -- No expansion needed, maps to (3,1)-(3,6) unchanged
        let ers = prepareEdits edits
        convertRange ers (r 3 1 3 6) @?= r 3 1 3 6
    ]
  where
    edits =
      [ Edit (r 3 6 3 7) "{! !}",
        Edit (r 4 1 4 2) "[!\n\n!]",
        Edit (r 5 6 5 7) "{! !}",
        Edit (r 6 1 6 2) "[!\n\n!]",
        Edit (r 7 6 7 7) "{! !}"
      ]
