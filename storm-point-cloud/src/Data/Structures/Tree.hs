{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Data.Structures.Tree where

import Data.Structures.Image
import Data.Structures.PointCloud
import Data.Utils
import Data.Matrix as M
import Data.Structures.PCBitStream

type TriForceRange = BinTree Range
type TriForceRangeTree = BinTree TriForceRange
type TriForceTree = BinTree (BinTree ImageSparse)
type TriForcePresenceTree = BinTree (BinTree [Presence])
type TriForcePresence = BinTree [Presence]
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

rangeSparseTree :: Range -> BinTree (Range, [Presence])
rangeSparseTree (i, j)
  | i == j    = Leaf ((i,j), [])
  | otherwise = Node ((i,j), []) (rangeSparseTree (i, j')) (rangeSparseTree (j' + 1, j))
        where j' = (j + i) `div` 2

rangeTriForce :: Range -> TriForceRange
rangeTriForce (a,b) = Node (a,b) (Leaf (a, b')) (Leaf (b' + 1, b))
    where b' = (b + a) `div` 2

triForceTreeRange :: Range -> BinTree TriForceRange
triForceTreeRange (a,b)
  | b - a == 1 = Leaf (rangeTriForce (a,b))
  | otherwise  = Node
                 (rangeTriForce (a,b))
                 (triForceTreeRange  (a, b'))
                 (triForceTreeRange  (b' + 1, b))
  where b' = (b + a) `div` 2

pc2TriForce :: Axis -> PointCloud -> Either String (TriForceTree, PointCloudSize)
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

-- computeLeft :: Father -> Left -> Binary -> (Left, Rest)
computeLeft :: Root -> [Presence] -> Bin -> ([Presence], Bin)
computeLeft [] e rest = (e, rest)
computeLeft (m:ms) e bs
    | m         = computeLeft ms (e ++ [head bs == 1]) (tail bs)
    | otherwise = computeLeft ms (e ++ [False]) bs

-- computeRight :: Father -> Left -> Right -> Binary -> (Right, Rest)
computeRight :: Root -> Left -> [Presence] -> Bin -> ([Presence], Bin)
computeRight [] [] e rest = (e, rest)
computeRight (m:ms) (l:ls) e bs
  | not m       = computeRight ms ls (e ++ [False]) bs
  | m && not l  = computeRight ms ls (e ++ [True]) bs
  | otherwise   = computeRight ms ls (e ++ [head bs == 1]) (tail bs)

computeTriForce :: Root -> Bin -> (TriForcePresence, Bin)
computeTriForce root b = (Node root (Leaf left) (Leaf right), b'')
  where (left, b') = computeLeft root [] b
        (right, b'') = computeRight root left [] b'

-- tf2tfp :: PointCloud -> TriForceRangeTree -> Root -> Bin -> (PointCloud, TriForcePresenceTree, Bin)
triForceR2PC :: PointCloud -> TriForceRangeTree -> Root -> Bin -> Header -> (PointCloud, TriForcePresenceTree, Bin)
triForceR2PC pc (Leaf tr) root b h = (leafNodes2PC pc tr pL pR h, Leaf (Node pV (Leaf pL) (Leaf pR)), b')
  where (Node pV (Leaf pL) (Leaf pR), b') = computeTriForce root b
triForceR2PC pc (Node _ tfrL tfrR) root b h = (pc'', Node (Node tfpV (Leaf tfpL) (Leaf tfpR)) tfTpL tfTpR, b''')
  where (Node tfpV (Leaf tfpL) (Leaf tfpR), b') = computeTriForce root b
        (pc',tfTpL, b'') = triForceR2PC pc tfrL tfpL b' h
        (pc'',tfTpR, b''') = triForceR2PC pc' tfrR tfpR b'' h

leafNodes2PC :: PointCloud -> TriForceRange -> [Presence] -> [Presence] -> Header -> PointCloud
leafNodes2PC pc (Node _ (Leaf rL) (Leaf rR)) leftL rightL h = pc''
  where axis = axisH h
        side = pcSizeH h
        pc' = addRasterToPointCloud rL axis (presenceList2Raster side leftL) pc
        pc'' = addRasterToPointCloud rR axis (presenceList2Raster side rightL) pc'