#! /usr/bin/env runhaskell
--
-- tests/runtest.hs - Test module for diff algorithm collection
--
-- Copyright (C) 2008, Hiroki Hattori
-- Licensed under BSD3, see COPYING
--
import Test.HUnit

import Data.Diff
import qualified Data.Diff.Algorithm.ONM as ONM
import qualified Data.Diff.Algorithm.ONP as ONP


main :: IO ()
main = runTestTT tests >> return ()
    where
      tests = test [
               "O(NM) diff" ~: testDiff ONM.diff,
               "O(NP) diff" ~: testDiff ONP.diff,
               -- "O(NM) hunk" ~: testHunk ONM.diff, -- 遅すぎて終わらないからパス
               "O(NP) hunk" ~: testHunk ONP.diff
              ]
      testDiff diff =
          [
           diff' "abcde" "abcde" [Right 5],
           diff' "abcde" "abcdf" [Right 4, Left (1,1)],
           diff' "abcde" "zbcde" [Left (1,1),Right 4],
           diff' "abcde" "abzde" [Right 2, Left (1,1), Right 2],
           diff' "abcde" "zbcdx" [Left (1,1), Right (3), Left (1,1)],
           diff' "abcde" "zabcde" [Left (0, 1), Right 5],
           diff' "abcde" "abcdez" [Right 5, Left (0,1)],
           diff' "abcde" "abczde" [Right 3, Left (0,1),Right 2]
          ]
          where diff' x y z = (x ++ " -> " ++ y ) ~: diff x y ~?= z
      testHunk diff =
          [
           (hunk2new $ path2hunk xs ys $ diff xs ys) ~?= ys,
           (hunk2old $ path2hunk xs ys $ diff xs ys) ~?= xs
          ]
          where xs = "The quick blown fox jumps over the lazy dog."
                ys = "The lazy black compiler jump over the function."
