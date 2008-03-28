#! /runhaskell
--
-- hdiff - A simple diff (1) implementation of haskell.
--
-- Copyright (C) 2008 Hiroki Hattori
-- Licensed under BSD3
--
module Main where

import IO
import System
import Text.Patch


main =
    do
      (fname1:fname2:[]) <- getArgs
      diffFile (Just 3) fname1 fname2
                   >>= putStr . unlines . ((++) ["--- " ++ fname1, "+++ " ++ fname2 ]) . concatMap formatDiff

