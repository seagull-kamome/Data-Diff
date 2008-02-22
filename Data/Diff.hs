--
-- Data/Diff.hs - Base module for diff algorithms
--
-- Copyright (C) 2008, Hiroki Hattori
-- Licensed under BSD3, see COPYING
--
module Data.Diff
    (
     Node, EditPath, PathOpr,
     Hunk, Patch,
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
     path2hunk, hunkNewList, hunkNewLength, hunkOldList, hunkOldLength,
     --
     inverseHunk, inversePatch,
     path2patch,
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
-- * Hunk操作
--

type Chunk a = (Int, [a])
type Hunk a = Either (Chunk a, Chunk a) (Chunk a)


-- | Hunk を逆転する
inverseHunk :: Hunk a -> Hunk a
inverseHunk (Left (n, m)) = Left (m, n)
inverseHunk x = x


-- | 編集パスをHunkのリストにする
path2hunk :: [a]        -- ^ 旧リスト
          -> [a]        -- ^ 新リスト
          -> EditPath   -- ^ 編集パス
          -> [Hunk a]   -- ^ 変形した結果 Hunk のリスト
path2hunk _ _ [] = []
path2hunk xs ys (Left (del, ins):path) = Left ((del, xs'), (ins, ys')) : path2hunk xs'' ys'' path
    where (xs', xs'') = splitAt del xs 
          (ys', ys'') = splitAt ins ys
path2hunk xs ys (Right cmn:path) = Right (cmn, xs') : path2hunk xs'' (drop cmn ys) path
    where (xs', xs'') = splitAt cmn xs 



-- | Hunkのリストから旧リストを取り出す
hunkOldList :: [Hunk a] -> [a]
hunkOldList = concatMap (either (snd . fst) snd)

-- | Hunkのリストから新リストを取り出す
hunkNewList :: [Hunk a] -> [a]
hunkNewList = concatMap (either (snd . snd) snd)

-- | Hunk のリストから旧リストの長さを算出する
hunkOldLength :: [Hunk a] -> Int
hunkOldLength = sum . map (either (fst . fst) fst)

-- | Hunk のリストから新リストの長さを算出する
hunkNewLength :: [Hunk a] -> Int
hunkNewLength = sum . map (either (fst . snd) fst)





--
-- * パッチ操作
--

-- | パッチ型
type Patch a = [(Int, Int, Hunk a)]


-- | パッチを逆転する
inversePatch :: Patch a -> Patch a
inversePatch = map $ \(n, m, hnk) -> (m, n, inverseHunk hnk)




-- | 編集パスをパッチにする
path2patch :: Maybe Int   -- ^ 編集部分の前後に含める共通部分の個数 Nothing ならすべて含める
           -> [a]         -- ^ 旧リスト
           -> [a]         -- ^ 新リスト
           -> EditPath    -- ^ 編集パス
           -> Patch a      -- ^ 生成されたパッチ
path2patch Nothing oldlist newlist paths = f 1 1 oldlist newlist paths
    where
      f _ _ [] [] [] = []
      f _ _ _ _ [] = error "No enogh old or new list."
      f n m xs ys (Right cnm:paths') =
          let (xs', xs'') = splitAt cnm xs
              (_ ,  ys'') = splitAt cnm ys
          in (n, m, Right (cnm, xs')) :  f (n + cnm) (m + cnm) xs'' ys'' paths'
      f n m xs ys (Left (del, ins):paths') =
          let (xs', xs'') = splitAt del xs
              (ys', ys'') = splitAt ins ys
          in (n, m, Left ((del, xs'), (ins, ys'))) : f (n + del) (m + ins) xs'' ys'' paths'

{-
path2patch (Just cl) oldlist newlist path = f0 x0 path 1 oldlist 1 newlist
    where
      isLeft = either (const True) (const False)
      isRight = not . isLeft
      takeDifference = either (const True) ((<) (cl * 2))
      (x0, y0) = span isRight path
      f0 xs {- 先行する共通部分のパス -} ys {- パス -} zn {- 旧リスト上の位置 -} zs {- 旧リスト -} wn {- 新リスト上の位置 -} ws {- 新リスト -}
          | null ds  = []
          | otherwise = hnk : f0 xs' ys'' zn' zs' wn' ws'
          where
            (ds, ys') = span takeDifference ys
            (xs', ys'') = span isRight ys'
            hnk = 

-}
