{-# OPTIONS_GHC -XStandaloneDeriving -Wall #-}
--
-- Data/Diff.hs - Base module for diff algorithms
--
-- Copyright (C) 2008, Hiroki Hattori
-- Licensed under BSD3, see COPYING
--
module Data.Diff
    (
     Node, EditPath, PathOpr,
     Hunk, Diff (..),
     DiffAlgorithm, GenericDiffAlgorithm,
     --
     addInsertPath, addDeletePath, addCommonPath,
     nodeOldLength, nodeNewLength, nodeCommonLength,
     pathOldLength, pathNewLength,
     genericSnake,
     --
     path2lcs, genericLcs,
     --
     inverseNode, inverseEditPath, normalizeEditPath,
     --
     inverseHunk, pathToHunk, hunkList, hunkNewList, hunkOldList,
     --
     inverseDiff, pathToDiff,
     diffOldList, diffOldLength, diffNewList, diffNewLength,
     --
     patch,
    ) where
--

import Data.List (mapAccumL, isPrefixOf)
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

normalizeEditPath :: EditPath -> EditPath
normalizeEditPath [] = []
normalizeEditPath xs@(_:[]) = xs
normalizeEditPath (Left (d1, i1):Left (d2, i2):xs) = normalizeEditPath $ Left (d1 + d2, i1 + i2) : xs
normalizeEditPath (Right c1:Right c2:xs) = normalizeEditPath $ Right (c1 + c2) : xs
normalizeEditPath (x:xs) = x : normalizeEditPath xs


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
-- * Hunk操作
--
type Hunk a = Either ([a], [a]) [a]


-- | Hunk を逆転する
inverseHunk :: Hunk a -> Hunk a
inverseHunk (Left (n, m)) = Left (m, n)
inverseHunk x = x


-- | 編集パスをDiffのリストにする
pathToHunk :: [a]        -- ^ 旧リスト
           -> [a]        -- ^ 新リスト
           -> EditPath   -- ^ 編集パス
           -> [Hunk a]  -- ^ 変形した結果
pathToHunk _ _ [] = []
pathToHunk xs ys (Left (del, ins):path) = Left (xs', ys') : pathToHunk xs'' ys'' path
    where (xs', xs'') = splitAt del xs 
          (ys', ys'') = splitAt ins ys
pathToHunk xs ys (Right cmn:path) = Right xs' : pathToHunk xs'' (drop cmn ys) path
    where (xs', xs'') = splitAt cmn xs 


-- |
hunkList :: Hunk a -> ([a], [a])
hunkList = either id (\x -> (x,x))

-- | Hunkのリストから旧リストを取り出す
hunkOldList :: Hunk a -> [a]
hunkOldList = either fst id

-- | Hunkのリストから新リストを取り出す
hunkNewList :: Hunk a -> [a]
hunkNewList = either snd id





--
-- * Diff操作
--

-- | Diff型
data Diff a = Diff {
      diffOldIndex  :: Int,
      diffNewIndex  :: Int,
      diffDiff      :: [Hunk a]
    }

deriving instance Show a => Show (Diff a)
deriving instance Eq a => Eq (Diff a)


-- | Diffを逆転する
inverseDiff :: Diff a -> Diff a
inverseDiff (Diff x1 x2 x3) = Diff x2 x1 $ map inverseHunk x3




-- | 編集パスをDiffにする
pathToDiff :: Maybe Int   -- ^ 編集部分の前後に含める共通部分の個数 Nothing ならすべて含める
           -> [a]         -- ^ 旧リスト
           -> [a]         -- ^ 新リスト
           -> EditPath    -- ^ 編集パス
           -> [Diff a]    -- ^ 生成されたDiff
pathToDiff _ _ _ []    = []
pathToDiff Nothing oldlist newlist paths = [Diff 1 1 $ snd $ mapAccumL f (oldlist, newlist) $ normalizeEditPath paths ]
    where
      f (ys, zs) = either
                   (\(d, i) ->let (y, ys') = splitAt d ys
                                  (z, zs') = splitAt i zs
                              in ((ys', zs'), Left (y, z))  )
                   (\c -> let (y, ys') = splitAt c ys in ((ys', drop c zs), Right (y)))

pathToDiff (Just cl) oldlist newlist path = f0 oldlist newlist $ normalizeEditPath path
    where
      f0 _ _ [] = []
      f0 _ _ (Right _:[]) = []
      f0 xs ys zs@(Left _:_) = f1 (\ds -> [Diff 0 0 ds]) xs 0 ys 0 zs
      f0 xs ys (Right c:zs) = f1 (\ds -> [Diff skip skip (Right (drop (c - cl) xs') : ds)] ) xs'' c (drop c ys) c zs
          where (xs', xs'') = splitAt c xs
                skip = max 0 $ c - cl
      f1 prefix _ _ _ _ [] = prefix []
      f1 prefix xs xn ys yn (Left (d, i) : zs) = f1 (prefix . (:) (Left (xs', ys'))) xs'' (xn + d) ys'' (yn + i) zs
          where
            (xs', xs'') = splitAt d xs
            (ys', ys'') = splitAt i ys
      f1 prefix xs xn ys yn (Right c : zs)
         | c <= cl * 2 = f1 (prefix . (:) (Right xs')) xs'' (xn + c) (drop c ys) (yn + c) zs
         | null zs   = prefix [ Right $ take cl xs ]
         | otherwise =
             f1 (\ds -> prefix [Right $ take cl xs] ++ [Diff (xn + skip) (yn + skip) (Right (drop skip xs') : ds)]) xs'' (xn + c) (drop c ys) (yn + c) zs
         where
           (xs', xs'') = splitAt c xs
           skip = c - cl



-- |
diffOldList :: Diff a -> [a]
diffOldList (Diff _ _ xs) = concatMap (either fst id) xs


-- |
diffOldLength :: Diff a -> Int
diffOldLength (Diff _ _ xs) = sum $ map (length . either fst id) xs

-- |
diffNewList :: Diff a -> [a]
diffNewList (Diff _ _ xs) = concatMap (either snd id) xs

-- |
diffNewLength :: Diff a -> Int
diffNewLength (Diff _ _ xs) = sum $ map (length . either snd id) xs



-- |
patch :: Eq a => [Diff a] -> [a] -> [a]
patch diff oldlist = patch' diff oldlist 0
    where
      patch' [] ys _ = ys
      patch' (x@(Diff oi _ _):xs) ys n
          | oi < n                  = error "patch: error. oi < n"
          | isPrefixOf oldchunk ys' = skiped ++ newchunk ++ patch' xs (drop oldlen ys') (oi + oldlen)
          | otherwise               = error "patch: error. prefix unmatch."
          where
            (skiped, ys') = splitAt (oi - n) ys
            oldchunk = diffOldList x
            oldlen  = length oldchunk
            newchunk = diffNewList x
