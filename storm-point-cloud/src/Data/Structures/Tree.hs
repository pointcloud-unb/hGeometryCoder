{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Data.Structures.Tree where

import Data.Structures.Image
import Data.Structures.PointCloud
import Data.Utils
import Data.Matrix as M
import Data.Structures.PCBitStream

type RangeTriForce = BinTree Range
type IRasterTriForce = BinTree [Occupancy]
type ISparseTriForce = BinTree ImageSparse
type RangeTriForceTree = BinTree RangeTriForce
type IRasterTriForceTree = BinTree IRasterTriForce
type ISparseTriForceTree = BinTree ISparseTriForce

data BinTree a = Node { nodeValue :: a
                      , left      :: BinTree a
                      , right     :: BinTree a }
               | Leaf { leafValue :: a}
               deriving (Eq, Show, Functor, Foldable)

getValue :: BinTree a -> a
getValue (Node nV l r) = nV
getValue (Leaf lV) = lV

rangeTree :: Range -> BinTree Range
rangeTree (i, j)
  | i == j    = Leaf (i,j)
  | otherwise = Node (i,j) (rangeTree (i, j')) (rangeTree (j' + 1, j))
  where j' = (j + i) `div` 2

rangeSparseTree :: Range -> BinTree (Range, [Occupancy])
rangeSparseTree (i, j)
  | i == j    = Leaf ((i,j), [])
  | otherwise = Node ((i,j), []) (rangeSparseTree (i, j')) (rangeSparseTree (j' + 1, j))
        where j' = (j + i) `div` 2

rangeTriForce :: Range -> RangeTriForce
rangeTriForce (a,b) = Node (a,b) (Leaf (a, b')) (Leaf (b' + 1, b))
    where b' = (b + a) `div` 2

triForceTreeRange :: Range -> BinTree RangeTriForce
triForceTreeRange (a,b)
  | b - a == 1 = Leaf (rangeTriForce (a,b))
  | otherwise  = Node
                 (rangeTriForce (a,b))
                 (triForceTreeRange  (a, b'))
                 (triForceTreeRange  (b' + 1, b))
  where b' = (b + a) `div` 2

pc2TriForce :: Axis -> PointCloud -> Either String (ISparseTriForceTree, PointCloudSize)
pc2TriForce axis pc = Right (fmap (f pc) <$> triForceTreeRange (0, pcSize pc - 1), pcSize pc)
  where f = \pc range -> sliceToSilhoutte axis . slicePointCloud' axis range $ pc

-- import Data.Either
-- import System.IO.Unsafe
-- pc = fromRight (PointCloud [] 0 0) $ unsafePerformIO main
-- f = \pc range -> sparseToRaster . sliceToSilhoutte X . slicePointCloud' X range $  pc
-- range = (0,3)
-- f <$> rangeTree range
-- (fmap.fmap) f $ triForceTree range

-- Decoder

-- computeLeftSilhouette :: Father -> Left -> Binary -> (Left, Rest)
computeLeftSilhouette :: TriforceRoot -> [Occupancy] -> Bin -> ([Occupancy], Bin)
computeLeftSilhouette [] e rest = (e, rest)
computeLeftSilhouette (m:ms) e bs
    | m         = computeLeftSilhouette ms (e ++ [head bs == 1]) (tail bs)
    | otherwise = computeLeftSilhouette ms (e ++ [False]) bs

-- computeRightSilhouette :: Father -> Left -> Right -> Binary -> (Right, Rest)
computeRightSilhouette :: TriforceRoot -> Left -> [Occupancy] -> Bin -> ([Occupancy], Bin)
computeRightSilhouette [] [] e rest = (e, rest)
computeRightSilhouette (m:ms) (l:ls) e bs
  | not m       = computeRightSilhouette ms ls (e ++ [False]) bs
  | m && not l  = computeRightSilhouette ms ls (e ++ [True]) bs
  | otherwise   = computeRightSilhouette ms ls (e ++ [head bs == 1]) (tail bs)

rTriForce2IRTriForce :: TriforceRoot -> Bin -> (IRasterTriForce, Bin)
rTriForce2IRTriForce root b = (Node root (Leaf left) (Leaf right), b'')
  where (left, b') = computeLeftSilhouette root [] b
        (right, b'') = computeRightSilhouette root left [] b'

-- tf2tfp :: PointCloud -> RangeTriForceTree -> TriforceRoot -> Bin -> (PointCloud, IRasterTriForceTree, Bin)
triForceR2PC :: PointCloud -> RangeTriForceTree -> TriforceRoot -> Bin -> Header -> (PointCloud, IRasterTriForceTree, Bin)
triForceR2PC pc (Leaf rT) root b h = (leafNodes2PC pc rT irL irR h, Leaf (Node irC (Leaf irL) (Leaf irR)), b')
  where (Node irC (Leaf irL) (Leaf irR), b') = rTriForce2IRTriForce root b
triForceR2PC pc (Node _ rTfTreeL rTfTreeR) root b h = (pc'', Node (Node irTfC (Leaf irTfL) (Leaf irTfR)) irTfTreeL irTfTreeR, b''')
  where (Node irTfC (Leaf irTfL) (Leaf irTfR), b') = rTriForce2IRTriForce root b
        (pc',irTfTreeL, b'') = triForceR2PC pc rTfTreeL irTfL b' h
        (pc'',irTfTreeR, b''') = triForceR2PC pc' rTfTreeR irTfR b'' h

leafNodes2PC :: PointCloud -> RangeTriForce -> [Occupancy] -> [Occupancy] -> Header -> PointCloud
leafNodes2PC pc (Node _ (Leaf rL) (Leaf rR)) leftL rightL h = pc''
  where axis = axisH h
        side = pcSizeH h
        pc' = addRasterToPointCloud rL axis (presenceList2Raster side leftL) pc
        pc'' = addRasterToPointCloud rR axis (presenceList2Raster side rightL) pc'