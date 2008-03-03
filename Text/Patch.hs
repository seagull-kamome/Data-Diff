--
-- Text/Patch.hs - Patch file manipulation module
--
-- Copyright (C) 2008, Hiroki Hattori
-- Licensed under BSD3, see COPYING
--
module Text.Patch (
                   Hunk, Diff, Patch (..),
                   diff, diffFile, formatDiff, readDiff,
--                   mkChangeset,
                  ) where

import Control.Monad ((>=>))
import Data.Maybe
import Data.List (unfoldr, span, partition)
import IO
import System.Directory (doesFileExist)
import Text.Regex

import qualified Data.Diff (Hunk, Diff (..), pathToDiff, hunkOldLength, hunkNewLength)
import qualified Data.Diff.Algorithm.ONP (genericDiff)


type Hunk = Data.Diff.Hunk String
type Diff = Data.Diff.Diff String

data Patch = Patch {
      patchOldName    :: String,
      patchNewName    :: String,
      patchBody       :: [Diff]
    }



-- | 文字列のリストの間の差分を取得する
diff :: Maybe Int -> [String] -> [String] -> [Diff]
diff cl oldlist newlist =
    Data.Diff.pathToDiff cl oldlist newlist $ Data.Diff.Algorithm.ONP.genericDiff (==) oldlist newlist



-- | ファイル間の差分を取得する
diffFile :: Maybe Int -> FilePath -> FilePath -> IO [Diff]
diffFile cmnlen oldfname newfname =
    do
      withf2 oldfname newfname
                 (\old new -> return $! Data.Diff.pathToDiff cmnlen old new $ Data.Diff.Algorithm.ONP.genericDiff (==) old new)
    where
      withf2 path1 path2 f = withf path1 (\txt1 -> withf path2 (\txt2 -> f txt1 txt2))
      withf path f = 
          doesFileExist path >>= (\b -> if b
                                   then bracket (openFile path ReadMode) hClose (hGetContents >=> f .lines)
                                   else f [])
                      

-- | 差分を書式化する
formatDiff :: Diff -> [String]
formatDiff (Data.Diff.Diff n m hunks) =
    (concat [ "@@ -", show $ n + 1, ",", show (Data.Diff.hunkOldLength hunks), " +", show $ m + 1, ",", show (Data.Diff.hunkNewLength hunks), " @@" ]) : (concatMap f' hunks)
    where
      f' x = case x of
               Right xs      -> map ((:) ' ') xs
               Left (xs, ys) -> map ((:) '-') xs ++ map ((:) '+') ys

-- |
readHunk :: [String] -> Maybe (Hunk, [String])
readHunk xs | not $ null cmn = Just (Right (map tail cmn), xs')
            | not $ null edt = Just (Left ((map tail del), (map tail ins)), xs'')
            | otherwise      = Nothing
    where (cmn, xs') = span ((==) ' ' . head) xs
          (edt, xs'') = span (flip elem "+-" . head) xs'
          (ins, del) = partition ((==) '+' . head) edt

-- | 差分をパースする
readDiff :: [String] -> Maybe (Diff, [String])
readDiff [] = Nothing
readDiff (x:xs') = matchRegex (mkRegex "^@@ -([0-9]+),([0-9]+) \\+([0-9]+),([0-9]+) @@$") x
                    >>= return . map read
                    >>= (\(h1:h2:h3:h4:[]) -> if oldlen == h2 && newlen == h4
                                              then return (Data.Diff.Diff h1 h3 hunks, xs'')
                                              else error "Hunk length missmatch")
    where (xs'', hunks') = readHunks xs' []
          hunks = reverse hunks'
          oldlen = Data.Diff.hunkOldLength hunks
          newlen = Data.Diff.hunkNewLength hunks
          readHunks xs ys
              | null xs        = (xs, ys)
              | not $ null cmn = readHunks xs' $! Right (map tail cmn) : ys
              | not $ null edt = readHunks xs'' $! Left ((map tail del), (map tail ins)) : ys
              | otherwise      = (xs, ys)
              where (cmn, xs') = span ((==) ' ' . head) xs
                    (edt, xs'') = span (flip elem "+-" . head) xs'
                    (ins, del) = partition ((==) '+' . head) edt

