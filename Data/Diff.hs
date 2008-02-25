{-# GHC_OPTIONS -XStandaloneDeriving #-}
--
-- Data/Diff.hs - Base module for diff algorithms
--
-- Copyright (C) 2008, Hiroki Hattori
-- Licensed under BSD3, see COPYING
--
module Data.Diff
    (
     Node, EditPath, PathOpr,
     Diff, Patch (..),
     DiffAlgorithm, GenericDiffAlgorithm,
     --
     addInsertPath, addDeletePath, addCommonPath,
     nodeOldLength, nodeNewLength, nodeCommonLength,
     pathOldLength, pathNewLength,
     genericSnake,
     --
     path2lcs, genericLcs,
     --
     inverseNode, inverseEditPath,
     --
     inverseDiff, pathToDiff, diffNewList, diffNewLength, diffOldList, diffOldLength,
     --
     inversePatch, pathToPatch,
    ) where
--

import Data.Either

--
-- * 編集パス操作
--
type Node = Either (Int {-del-}, Int {-ins-}) Int {-common-}
type EditPath = [Node]
type PathOpr = Int -> EditPath -> EditPath


inverseNode :: Node -> Node
inverseNode (Left (del, ins)) = Left (ins, del)
inverseNode x = x

inverseEditPath :: EditPath -> EditPath
inverseEditPath = map inverseNode


addInsertPath :: PathOpr
addInsertPath 0 path = path
addInsertPath n []   = [Left (0, n)]
addInsertPath n path@(Right _:_) = Left (0, n) : path
addInsertPath n (Left (del, ins):path) = Left (del, ins + n) : path


addDeletePath :: PathOpr
addDeletePath 0 path = path
addDeletePath n []   = [Left (n, 0)]
addDeletePath n path@(Right _:_) = Left (n, 0) : path
addDeletePath n (Left (del, ins):path) = Left (del + n, ins) : path


addCommonPath :: PathOpr
addCommonPath 0 path = path
addCommonPath n [] = [Right n]
addCommonPath n path@(Left _:_) = Right n : path
addCommonPath n (Right cmn:path) = Right (cmn + n) : path


nodeOldLength :: Node -> Int
nodeOldLength = either fst id

nodeNewLength :: Node -> Int
nodeNewLength = either snd id

nodeCommonLength :: Node -> Int
nodeCommonLength = either (const 0) id


pathOldLength :: EditPath -> Int
pathOldLength = sum . map nodeOldLength

pathNewLength :: EditPath -> Int
pathNewLength = sum . map nodeNewLength




--
--
--
genericSnake :: (a -> a -> Bool) ->  [a] -> [a] -> (Int, [a], [a])
genericSnake cmp = snake' 0
    where
      snake' n xs [] = (n, xs, [])
      snake' n [] ys = (n, [], ys)
      snake' n xs@(x:xs') ys@(y:ys')
          | cmp x y   = snake' (n + 1) xs' ys'
          | otherwise = (n, xs, ys)



--
--
--
type DiffAlgorithm a = [a] -> [a] -> EditPath
type GenericDiffAlgorithm a = (a -> a -> Bool) -> DiffAlgorithm a


--
-- * LCS 操作
--

-- | 編集パスを lcs にする
path2lcs :: [a]       -- ^ 旧リスト
         -> [a]       -- ^ 新リスト
         -> EditPath  -- ^ 編集パス
         -> [[a]]     -- ^ 共通部分のみのリスト
path2lcs _ _ [] = []
path2lcs oldlist newlist (Left (n, m):paths) = path2lcs (drop n oldlist) (drop m newlist) paths
path2lcs oldlist newlist (Right n:paths) = take n oldlist : path2lcs (drop n oldlist) (drop n newlist) paths


-- | アルゴリズムを指定して、２つのリストのlcsを算出する
genericLcs :: DiffAlgorithm a  -- ~ 差分アルゴリズム
           -> [a]              -- ^ 最初のリスト
           -> [a]              -- ^ 二番目のリスト
           -> [[a]]            -- ^ 共通部分リストのリスト

genericLcs f x y = path2lcs x y $ f x y







--
-- * Diff操作
--
type Diff a = Either ([a], [a]) [a]


-- | DIff を逆転する
inverseDiff :: Diff a -> Diff a
inverseDiff (Left (n, m)) = Left (m, n)
inverseDiff x = x


-- | 編集パスをDiffのリストにする
pathToDiff :: [a]        -- ^ 旧リスト
           -> [a]        -- ^ 新リスト
           -> EditPath   -- ^ 編集パス
           -> [Diff a]  -- ^ 変形した結果 Chunk のリスト
pathToDiff _ _ [] = []
pathToDiff xs ys (Left (del, ins):path) = Left (xs', ys') : pathToDiff xs'' ys'' path
    where (xs', xs'') = splitAt del xs 
          (ys', ys'') = splitAt ins ys
pathToDiff xs ys (Right cmn:path) = Right xs' : pathToDiff xs'' (drop cmn ys) path
    where (xs', xs'') = splitAt cmn xs 



-- | Diffのリストから旧リストを取り出す
diffOldList :: [Diff a] -> [a]
diffOldList = concatMap (either fst id)

-- | Diffのリストから新リストを取り出す
diffNewList :: [Diff a] -> [a]
diffNewList = concatMap (either snd id)

-- | Diffのリストから旧リストの長さを算出する
diffOldLength :: [Diff a] -> Int
diffOldLength = sum . map (length . either fst id)

-- | Diffのリストから新リストの長さを算出する
diffNewLength :: [Diff a] -> Int
diffNewLength = sum . map (length . either snd id)





--
-- * パッチ操作
--

-- | パッチ型
data Patch a = Patch {
      patchOldIndex  :: Int,
      patchNewIndex  :: Int,
      patchOldLength :: Int,
      patchNewLength :: Int,
      patchDiff      :: [Diff a]
    }

deriving instance Show a => Show (Patch a)


-- | パッチを逆転する
inversePatch :: Patch a -> Patch a
inversePatch (Patch x1 x2 x3 x4 x5) = Patch x2 x1 x4 x3 $ map inverseDiff x5




-- | 編集パスをパッチにする
pathToPatch :: Maybe Int   -- ^ 編集部分の前後に含める共通部分の個数 Nothing ならすべて含める
            -> [a]         -- ^ 旧リスト
            -> [a]         -- ^ 新リスト
            -> EditPath    -- ^ 編集パス
            -> [Patch a]  -- ^ 生成されたパッチ
pathToPatch Nothing oldlist newlist paths = [Patch 1 1 (pathOldLength paths) (pathNewLength paths) $ f oldlist newlist paths ]
    where
      f _ _ [] = []
      f xs ys (Right cmn:paths') =
          let (xs', xs'') = splitAt cmn xs
              (_ ,  ys'') = splitAt cmn ys
          in Right xs' :  f xs'' ys'' paths'
      f xs ys (Left (del, ins):paths') =
          let (xs', xs'') = splitAt del xs
              (ys', ys'') = splitAt ins ys
          in Left (xs', ys') : f xs'' ys'' paths'

pathToPatch (Just cl) oldlist newlist path = f1 path 1 oldlist 1 newlist
    where
      collectCommon ((Right x):xs) = let (n, xs') = collectCommon xs in (n + x, xs')
      collectCommon xs = (0, xs)
      collectDifference ((Left (del, ins)):xs) = let (n, m, xs') = collectDifference xs in (n + del, m + ins, xs')
      collectDifference xs = (0, 0, xs)
      --
      f1 :: EditPath -> Int -> [a] -> Int -> [a] -> [Patch a]
      f1 [] _ _ _ _ = []
      f1 xs yn ys zn zs =
          let (ds, tc, n, m, xs'', ys'', zs'') = f0 xs' ys' (drop cmnlen zs)
          in Patch (yn + cmnlen - cmnlen') (zn + cmnlen - cmnlen') (n + cmnlen' + tc) (m + cmnlen' + tc) (Right cmnlines : ds)
                 : f1 xs'' (yn + n) ys'' (zn + m)  zs''
         where
           cmnlen' = max cmnlen (cmnlen - cl)
           (cmnlen, xs') = collectCommon xs
           (cmnlines, ys') = splitAt cmnlen ys
      f0 :: EditPath -> [a] -> [a] -> ([Diff a], Int, Int, Int, EditPath, [a], [a])
      f0 [] ys zs = ([], 0, 0, 0, [], ys, zs)
      f0 xs ys  zs
          | cmnlen <= cl * 2 = let (ds, tc, n, m, xs''', ys''', zs''') = f0 xs'' ys'' zs''
                               in ((Left (dellines, inslines) : Right cmnlines : ds), tc, n + dellen + cmnlen, m + inslen + cmnlen, xs''', ys''', zs''')
          | otherwise        = ([Left (dellines, inslines), Right tailcmn], length tailcmn, dellen, inslen, xs', ys', zs')
          where
            (dellen, inslen, xs') = collectDifference xs
            (dellines, ys') = splitAt dellen ys
            (inslines, zs') = splitAt inslen zs
            (cmnlen, xs'') = collectCommon xs'
            (cmnlines, ys'') = splitAt cmnlen ys'
            zs'' = drop cmnlen zs'
            tailcmn = take cl ys'
