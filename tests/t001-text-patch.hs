#! /usr/bin/env runhaskell
--
-- tests/t001-text-patch.hs - Test module for Text.Patch
--
-- Copyright (C) 2008, Hiroki Hattori
-- Licensed under BSD3, see COPYING
--
import Test.HUnit

import Text.Patch

text_a =
    [ "Hello, World!",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10",
      "Last line"
    ]

text_b =
    [ "Hello",
      "1",
      "2",
      "3",
      "4",
      "a",
      "6",
      "7",
      "8",
      "9",
      "10",
      "Last line?"
    ]

text_c =
    [ "Hello, World!",
      "1",
      "2",
      "3",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10",
      "Last line?",
      "11"
    ]



main :: IO ()
main = runTestTT tests >> return ()
    where
      tests = test [
               "diff -U1 a b" ~:
                              formatDiff (diff (Just 1) text_a text_b) ~?= [
                                               "@@ -1,2 +1,2 @@",
                                               "-Hello, World!",
                                               "+Hello",
                                               " 1",
                                               "@@ -5,3 +5,3 @@",
                                               " 4",
                                               "-5",
                                               "+a",
                                               " 6",
                                               "@@ -11,2 +11,2 @@",
                                               " 10",
                                               "-Last line",
                                               "+Last line?"
                                              ]
              ]
