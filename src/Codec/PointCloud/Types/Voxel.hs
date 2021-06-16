{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module Codec.PointCloud.Types.Voxel where

import Codec.PointCloud.Utils
import qualified Data.ByteString as B

import Flat
import GHC.Generics (Generic)
import Control.DeepSeq


--class (Eq a, Ord a) => Voxel a where
--  unpack :: Voxel a -> (Coordinate, Coordinate, Coordinate)
--  largestDimension :: Voxel -> Int


-- Voxel type
data Voxel = Voxel { getU :: Coordinate
                   , getV :: Coordinate
                   , getW :: Coordinate }
  deriving (Show, Generic, Flat, NFData)


instance Eq Voxel where
  -- (==) Voxel -> Voxel -> Bool
  (Voxel u1 v1 w1) ==
    (Voxel u2 v2 w2) = u1 == u2 && v1 == v2 && w1 == w2

  -- (!=) Voxel -> Voxel -> Bool
  (Voxel u1 v1 w1) /=
    (Voxel u2 v2 w2) = u1 /= u2 || v1 /= v2 || w1 /= w2

instance Ord Voxel where
  -- Less comparison
  (Voxel u1 v1 w1) < (Voxel u2 v2 w2)
    = if u1 == u2 then
        if v1 == v2 then
          w1 < w2
        else
          v1 < v2
      else
        u1 < u2
  -- Less or Equal comparison
  (Voxel u1 v1 w1) <= (Voxel u2 v2 w2)
    = if u1 == u2 then
        if v1 == v2 then
          w1 <= w2
        else
          v1 <= v2
      else
        u1 <= u2
  -- Greater comparison
  (Voxel u1 v1 w1) > (Voxel u2 v2 w2)
    = if u1 == u2 then
        if v1 == v2 then
          w1 > w2
        else
          v1 > v2
      else
        u1 > u2
  -- Greater or Equal comparison
  (Voxel u1 v1 w1) >= (Voxel u2 v2 w2)
    = if u1 == u2 then
        if v1 == v2 then
          w1 >= w2
        else
          v1 >= v2
      else
        u1 >= u2
{- 
instance Semigroup Voxel where
(Voxel u1 v1 w1) <> (Voxel u2 v2 w2) = Voxel (max u1 u2) (max v1 v2) (max w1 w2)

instance Monoid Voxel where
    mempty = Voxel 0 0 0 -}

--maxLimit :: [Voxel] -> (Int, Int, Int)
--maxLimit v = (maximum $ map getU v, maximum $ map getV v, maximum $ map getW v)

--minLimit :: [Voxel] -> (Int, Int, Int)
--minLimit v = (minimum $ map getU v, minimum $ map getV v, minimum $ map getW v)

largestDimension :: Voxel -> Int
largestDimension (Voxel u v w) = maximum [u,v,w]

--pixelToVoxel :: Coordinate -> Coordinate -> Coordinate -> Voxel
--pixelToVoxel u v w = Voxel (u - 1) (v - 1) (w - 1)
