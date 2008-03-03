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
               "O(NP) genDiff" ~: testDiff ONP.diff
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
           (hunkNewList $ pathToHunk xs ys $ diff xs ys) ~?= ys,
           (hunkOldList $ pathToHunk xs ys $ diff xs ys) ~?= xs
          ]
      xs = "The quick blown fox jumps over the lazy dog."
      ys = "The lazy black compiler jump over the function."
      testDiff diff =
          [
           (pathToDiff (Just 1) xs ys $ diff xs ys) ~?= [Diff {diffOldIndex = 3, diffNewIndex = 3, diffDiff = [Right " ",Left ("quick","lazy"),Right " "]},Diff {diffOldIndex = 11, diffNewIndex = 10, diffDiff = [Right "l",Left ("own","ack"),Right " ",Left ("f","c"),Right "o",Left ("x","mpiler"),Right " "]},Diff {diffOldIndex = 23, diffNewIndex = 27, diffDiff = [Right "p",Left ("s",""),Right " "]},Diff {diffOldIndex = 34, diffNewIndex = 37, diffDiff = [Right " ",Left ("lazy d","functi"),Right "o",Left ("g","n"),Right "."]}],
           (pathToDiff (Just 0) xs ys $ diff xs ys) ~?= [Diff {diffOldIndex = 4, diffNewIndex = 4, diffDiff = [Left ("quick","lazy")]},Diff {diffOldIndex = 12, diffNewIndex = 11, diffDiff = [Left ("own","ack")]},Diff {diffOldIndex = 16, diffNewIndex = 15, diffDiff = [Left ("f","c")]},Diff {diffOldIndex = 18, diffNewIndex = 17, diffDiff = [Left ("x","mpiler")]},Diff {diffOldIndex = 24, diffNewIndex = 28, diffDiff = [Left ("s","")]},Diff {diffOldIndex = 35, diffNewIndex = 38, diffDiff = [Left ("lazy d","functi")]},Diff {diffOldIndex = 42, diffNewIndex = 45, diffDiff = [Left ("g","n")]}]
          ]