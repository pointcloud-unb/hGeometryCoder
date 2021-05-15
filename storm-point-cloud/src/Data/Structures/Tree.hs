{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Data.Structures.Tree where

import Data.Structures.Image
import Data.Structures.PointCloud
import Data.Utils

type TriForceRange = BinTree Range
type TriForceTree = BinTree (BinTree ImageSparse)
type TriForce = BinTree ImageSparse

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

rangeTriForce :: Range -> TriForceRange
rangeTriForce (a,b) = Node (a,b) (Leaf (a, b')) (Leaf (b' + 1, b))
    where b' = (b + a) `div` 2

triForceTree :: Range -> BinTree TriForceRange
triForceTree (a,b)
  | b - a == 1 = Leaf (rangeTriForce (a,b))
  | otherwise  = Node
                 (rangeTriForce (a,b))
                 (triForceTree  (a, b'))
                 (triForceTree  (b' + 1, b))
  where b' = (b + a) `div` 2

pc2TriForce :: Axis -> PointCloud -> Either String (TriForceTree, PointCloudSize)
pc2TriForce axis pc = Right (fmap (f pc) <$> triForceTree (0, pcSize pc - 1), pcSize pc)
  where f = \pc range -> sliceToSilhoutte axis . slicePointCloud' axis range $ pc

-- import Data.Either
-- import System.IO.Unsafe
-- pc = fromRight (PointCloud [] 0 0) $ unsafePerformIO main
-- f = \pc range -> sparseToRaster . sliceToSilhoutte X . slicePointCloud' X range $  pc
-- range = (0,3)
-- f <$> rangeTree range
-- (fmap.fmap) f $ triForceTree range
