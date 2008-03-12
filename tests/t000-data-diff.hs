#! /usr/bin/env runhaskell
--
-- tests/t000-data-diff.hs - Test module for diff algorithm collection
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
               "O(NM) diff" ~: testEditPath ONM.diff,
               "O(NP) diff" ~: testEditPath ONP.diff,
               "O(NP) hunk" ~: testHunk ONP.diff,
               "O(NP) genDiff" ~: testDiff ONP.diff,
               "patch" ~: testPatch
              ]
      testEditPath diff =
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
           (concatMap hunkNewList $ pathToHunk xs ys $ diff xs ys) ~?= ys,
           (concatMap hunkOldList $ pathToHunk xs ys $ diff xs ys) ~?= xs
          ]
      xs = "The quick blown fox jumps over the lazy dog."
      ys = "The lazy broken compiler jumps over the function."
      testDiff diff =
          [
           (pathToDiff (Just 1) xs ys $ diff xs ys) ~?= [Diff {diffOldIndex = 3, diffNewIndex = 3, diffDiff = [Right " ",Left ("quick","lazy"),Right " b",Left ("l","r"),Right "o",Left ("w","ke"),Right "n ",Left ("f","c"),Right "o",Left ("x","mpiler"),Right " "]},Diff {diffOldIndex = 34, diffNewIndex = 39, diffDiff = [Right " ",Left ("lazy d","functi"),Right "o",Left ("g","n"),Right "."]}],
           (pathToDiff (Just 0) xs ys $ diff xs ys) ~?= [Diff {diffOldIndex = 4, diffNewIndex = 4, diffDiff = [Right "",Left ("quick","lazy"),Right ""]},Diff {diffOldIndex = 11, diffNewIndex = 10, diffDiff = [Right "",Left ("l","r"),Right ""]},Diff {diffOldIndex = 13, diffNewIndex = 12, diffDiff = [Right "",Left ("w","ke"),Right ""]},Diff {diffOldIndex = 16, diffNewIndex = 16, diffDiff = [Right "",Left ("f","c"),Right ""]},Diff {diffOldIndex = 18, diffNewIndex = 18, diffDiff = [Right "",Left ("x","mpiler"),Right ""]},Diff {diffOldIndex = 35, diffNewIndex = 40, diffDiff = [Right "",Left ("lazy d","functi"),Right ""]},Diff {diffOldIndex = 42, diffNewIndex = 47, diffDiff = [Right "",Left ("g","n"),Right ""]}]
          ]
      testPatch =
          [
           patch (pathToDiff (Just 2) xs ys $ ONP.diff xs ys) xs ~?= ys
          ]

