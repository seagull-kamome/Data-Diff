--
-- Data/Diff/Algorithm/ONM - O(NM) diff algorithm module
--
-- Copyright (C) 2008, Hiroki Hattori
-- Licensed under BSD3, see COPYING
--
module Data.Diff.Algorithm.ONM
    (
     module Data.Diff,
     genericDiff, diff,
    ) where

import Data.Diff

diff :: Eq a => DiffAlgorithm a
diff = genericDiff (==)

genericDiff :: GenericDiffAlgorithm a
genericDiff cmp oldlist newlist = snd $ onm oldlist newlist
    where
      onm xs ys = let (cmn, xs', ys') = genericSnake cmp xs ys
                      (cst, path) = onm' xs' ys'
                  in (cst, addCommonPath cmn path)
      onm' [] [] = (0, [])
      onm' [] ys = let yn = length ys in (yn, addInsertPath yn [])
      onm' xs [] = let xn = length xs in (xn, addDeletePath xn [])
      onm' xs@(x:xs') ys@(y:ys') = let (c1, p1) = onm xs ys'
                                       (c2, p2) = onm xs' ys
                                   in if c1 < c2
                                      then (c1 + 1, addInsertPath 1 p1)
                                      else (c2 + 1, addDeletePath 1 p2)


