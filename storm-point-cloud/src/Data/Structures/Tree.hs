{-# LANGUAGE DeriveFunctor #-}

module Data.Structures.Tree where

import Data.Structures.Image

type Range = (Int, Int)
type TriForce = BinTree Range

data BinTree a = Node { nodeValue :: a
                      , left      :: BinTree a
                      , right     :: BinTree a }
               | Leaf { leafValue :: a}
               deriving (Eq, Show, Functor)

getValue :: BinTree a -> a
getValue (Node nV l r) = nV
getValue (Leaf lV) = lV

rangeTree :: Range -> BinTree Range
rangeTree (i, j)
  | i == j    = Leaf (i,j) 
  | otherwise = Node (i,j) (rangeTree (i, j')) (rangeTree (j' + 1, j))
  where j' = (j + i) `div` 2

rangeTriForce :: Range -> TriForce
rangeTriForce (a,b) = Node (a,b) (Leaf (a, b')) (Leaf (b' + 1, b))
    where b' = (b + a) `div` 2

triForceTree :: Range -> BinTree TriForce
triForceTree (a,b)
  | b - a == 1 = Leaf (rangeTriForce (a,b)) 
  | otherwise  = Node
                 (rangeTriForce (a,b))
                 (triForceTree  (a, b'))
                 (triForceTree  (b' + 1, b))
  where b' = (b + a) `div` 2

-- import Data.Either
-- import System.IO.Unsafe
-- pc = fromRight (PointCloud [] 0 0) $ unsafePerformIO main
-- f = \pc range -> sparseToRaster . sliceToSilhoutte X . slicePointCloud' X range $  pc
-- range = (0,3)
-- f <$> rangeTree range
-- (fmap.fmap) f $ triForceTree range
