{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Codec.PointCloud.Types.Tree where

import Codec.PointCloud.Types.Image
import Codec.PointCloud.Types.PointCloud
import Codec.PointCloud.Utils

import Data.Matrix as M

type RangeTriForce = BinTree Range
type OcuppancyTriForce = BinTree [Occupancy]
type ISparseTriForce = BinTree ImageSparse
type RangeTriForceTree = BinTree RangeTriForce
type OcuppancyTriForceTree = BinTree OcuppancyTriForce
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
computeLeftSilhouette :: TriForceRoot -> [Occupancy] -> Bin -> ([Occupancy], Bin)
computeLeftSilhouette [] e rest = (e, rest)
computeLeftSilhouette (m:ms) e bs
    | m         = computeLeftSilhouette ms (e ++ [head bs == 1]) (tail bs)
    | otherwise = computeLeftSilhouette ms (e ++ [False]) bs

-- computeRightSilhouette :: Father -> Left -> Right -> Binary -> (Right, Rest)
computeRightSilhouette :: TriForceRoot -> Left -> [Occupancy] -> Bin -> ([Occupancy], Bin)
computeRightSilhouette [] [] e rest = (e, rest)
computeRightSilhouette (m:ms) (l:ls) e bs
  | not m       = computeRightSilhouette ms ls (e ++ [False]) bs
  | m && not l  = computeRightSilhouette ms ls (e ++ [True]) bs
  | otherwise   = computeRightSilhouette ms ls (e ++ [head bs == 1]) (tail bs)

rTriForce2OTriForce :: TriForceRoot -> Bin -> (OcuppancyTriForce, Bin)
rTriForce2OTriForce root b = (Node root (Leaf left) (Leaf right), b'')
  where (left, b') = computeLeftSilhouette root [] b
        (right, b'') = computeRightSilhouette root left [] b'

-- triForceR2PC :: PointCloud -> RangeTriForceTree -> TriforceRoot -> Bin -> (PointCloud, IRasterTriForceTree, Bin)
triForceR2PC :: PointCloud -> RangeTriForceTree -> TriForceRoot -> Bin -> (Axis, PointCloudSize) -> (PointCloud, OcuppancyTriForceTree, Bin)
triForceR2PC pc (Leaf rT) root b h = (leafNodes2PC pc rT oL oR h, Leaf (Node oC (Leaf oL) (Leaf oR)), b')
  where (Node oC (Leaf oL) (Leaf oR), b') = rTriForce2OTriForce root b
triForceR2PC pc (Node _ rTfTreeL rTfTreeR) root b h = (pc'', Node (Node oTfC (Leaf oTfL) (Leaf oTfR)) oTfTreeL oTfTreeR, b''')
  where (Node oTfC (Leaf oTfL) (Leaf oTfR), b') = rTriForce2OTriForce root b
        (pc',oTfTreeL, b'') = triForceR2PC pc rTfTreeL oTfL b' h
        (pc'',oTfTreeR, b''') = triForceR2PC pc' rTfTreeR oTfR b'' h

leafNodes2PC :: PointCloud -> RangeTriForce -> [Occupancy] -> [Occupancy] -> (Axis, PointCloudSize) -> PointCloud
leafNodes2PC pc (Node _ (Leaf rL) (Leaf rR)) leftL rightL (axis, side) = pc''
  where pc' = addRasterToPointCloud rL axis (occupancyList2Raster side leftL) pc
        pc'' = addRasterToPointCloud rR axis (occupancyList2Raster side rightL) pc'
