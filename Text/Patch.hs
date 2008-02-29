--
-- Text/Patch.hs - Patch file manipulation module
--
-- Copyright (C) 2008, Hiroki Hattori
-- Licensed under BSD3, see COPYING
--
module Text.Patch (
                   Hunk, Diff, Patch (..),
--                   diff, diffFile, formatPatch, parsePatch,
--                   mkChangeset,
                  ) where

import IO
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
diff :: Maybe Int -> [String] -> [String] -> Patch
diff cl oldlist newlist =
    Patch "a" "b" $ Data.Diff.pathToDiff cl oldlist newlist $ Data.Diff.Algorithm.ONP.genericDiff (==) oldlist newlist



-- | ファイル間の差分を取得する
-- FIXME: ファイルが存在しない場合には、空リストとの差分を取る
diffFile :: Maybe Int -> FilePath -> FilePath -> IO Patch
diffFile cmnlen oldfname newfname =
    do
      bracket (openFile oldfname ReadMode)
              hClose
              (\hold ->
                   bracket (openFile newfname ReadMode)
                           hClose 
                           (\hnew ->
                                do
                                  old <- hGetContents hold >>= return . lines
                                  new <- hGetContents hnew >>= return . lines
                                  let ! diff = Data.Diff.pathToDiff cmnlen old new $ Data.Diff.Algorithm.ONP.genericDiff (==) old new
                                  return $ Patch oldfname newfname diff
                           )
              )


-- | 差分を書式化する
formatPatch :: Patch -> [String]
formatPatch (Patch x y xs) =
    [ replicate 3 '+' ++ " " ++ x,
      replicate 3 '-' ++ " " ++ y ] ++ concatMap f xs
    where
      f :: Data.Diff.Diff String -> [String]
      f (Data.Diff.Diff n m hunks) =
          (concat [ "@@ -", show $ n + 1, ",", show (Data.Diff.hunkOldLength hunks), " +", show $ m + 1, ",", show (Data.Diff.hunkNewLength hunks), " @@" ])
            : (concatMap f' hunks)
      f' :: Data.Diff.Hunk String -> [String]
      f' x = case x of
               Right xs      -> map ((:) ' ') xs
               Left (xs, ys) -> map ((:) '-') xs ++ map ((:) '+') ys


{-
-- | 差分をパースする
parseDiff :: [String] -> Diff




-- | 
data Changeset = ChangeSet [Patch]




-- | ファイル群を比較してチェンジセットを作る
mkChangesetFromFiles :: String -> String -> (String, String) -> Changeset
-}