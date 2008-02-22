--
-- Data/Diff/Algorithm/ONP.hs - O(NP) diff algorithm module
--
-- Copyright (C) 2008, Hiroki Hattori
-- Licensed under BSD3, see COPYING
--
-- References:
--  [1] E.W.Myers, "An O(ND) difference algorithm and its variations", Algorithmixa, 1 (1986), pp.251-266
--  [2] S.W.maner, "G.Myers, W.Miller, An O(NP) Sequence Comparison Algorith", August 1989
--  [3] 文書比較アルゴリズム http://hp.vector.co.jp/authors/VA007799/viviProg/doc5.htm
--  [4] diff (3) http://www.slash-zero.jp/archives/program/476
--
module Data.Diff.Algorithm.ONP
    (
     module Data.Diff,
     genericDiff, diff,
    ) where

import Control.Monad.Fix (fix)
import Data.Diff
import Debug.Trace


diff :: Eq a => DiffAlgorithm a
diff = genericDiff (==)

genericDiff :: GenericDiffAlgorithm a
genericDiff cmp oldlist newlist = let ((_, _,  _, path): _, _) = onp (fp_ab_0, []) in reverse path
    where
      --
      snake = genericSnake cmp
      -- 正規化した入力値
      (xn, xs, insopr, yn, ys, delopr)
          | oldlen <= newlen = (oldlen, oldlist, addInsertPath, newlen, newlist, addDeletePath)
          | otherwise        = (newlen, newlist, addDeletePath, oldlen, oldlist, addInsertPath)
          where
            oldlen = length oldlist
            newlen = length newlist
      -- 初期状態
      --    fp_a_p=fp[delta, p], fp_b_p=fp[(delta-1)..-p, p], fp_c_p=fp[delta+1..delta+p.p]
      --    とした時のp=0の時の状態 fp_ab_0=(fp_a_0:fp_b_0)。定義上、fp_c_0は常に[]。
      fp_ab_0 = reverse $ fix (\f (n, fp) -> fp:if n == 0 then [] else f ((n - 1), newfp_ins fp) ) (yn - xn {-delta-}, fp_0_0)
          where fp_0_0 = let (s, xs', ys') = snake xs ys in (s, xs', ys', addCommonPath s [])
      --
      newfp_ins (s, xs,     (_:ys), path) = let (n, x, y) = snake xs ys in (n + s + 1, x, y, addCommonPath n $ insopr 1 path)
      newfp_del (s, (_:xs), ys,     path) = let (n, x, y) = snake xs ys in (n + s,     x, y, addCommonPath n $ delopr 1 path)
      --
      -- O(NP)本体
      --
      onp fp@(fp_ab@(fp_a@(_, _, s_y, _):_), fp_c)
          | null s_y  = fp
          | otherwise = onp (newfp_a:newfp_b, newfp_c)
          where
            newfp_a = newfp (head newfp_b) (head newfp_c)    -- k == delta
            newfp_b = newfp_b' fp_ab                         -- k < delta
                where
                  newfp_b' (fp@(_, [], _, _):[])  = [] -- xがxnを越えるので刈る
                  newfp_b' (fp:[])  = [newfp_del fp]
                  newfp_b' (fp:fps) = let newfps = newfp_b' fps in newfp (head newfps) fp : newfps
            newfp_c = newfp_c' (fp_a:fp_c)                   -- k > delta
                where
                  newfp_b' (fp@(_, _, [], _):[])  = [] -- yがynを越えるので刈る
                  newfp_c' (fp:[])  = [newfp_ins fp]
                  newfp_c' (fp:fps) = let newfps = newfp_c' fps in newfp fp (head newfps) : newfps
            -- 
            newfp fp_l@(s_l, _, _, _) fp_r@(s_r, _, _, _)
                | s_l + 1 >= s_r = newfp_ins fp_l
                | otherwise      = newfp_del fp_r


