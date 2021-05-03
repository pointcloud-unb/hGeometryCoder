module Data.Structures.PointCloud where

import Data.List
import qualified Data.Structures.Image as Image

-- Voxel type
type Coordinate = Int
data Voxel = Voxel Coordinate Coordinate Coordinate
  deriving(Show)


instance Eq Voxel where
  -- (==) Voxel -> Voxel -> Bool
  (Voxel x1 y1 z1) ==
    (Voxel x2 y2 z2) = (x1 == x2) && (y1 == y2) && (z1 == z2)

  -- (!=) Voxel -> Voxel -> Bool
  (Voxel x1 y1 z1) /=
    (Voxel x2 y2 z2) = (x1 /= x2) || (y1 /= y2) || (z1 /= z2)

instance Ord Voxel where
  -- Less comparison
  (Voxel x1 y1 z1) < (Voxel x2 y2 z2)
    = if x1 == x2 then
        if y1 == y2 then
          z1 < z2
        else
          y1 < y2
      else
        x1 < x2
  -- Less or Equal comparison
  (Voxel x1 y1 z1) <= (Voxel x2 y2 z2)
    = if x1 == x2 then
        if y1 == y2 then
          z1 <= z2
        else
          y1 <= y2
      else
        x1 <= x2
  -- Greater comparison
  (Voxel x1 y1 z1) > (Voxel x2 y2 z2)
    = if x1 == x2 then
        if y1 == y2 then
          z1 > z2
        else
          y1 > y2
      else
        x1 > x2
  -- Greater or Equal comparison
  (Voxel x1 y1 z1) >= (Voxel x2 y2 z2)
    = if x1 == x2 then
        if y1 == y2 then
          z1 >= z2
        else
          y1 >= y2
      else
        x1 >= x2

-- Point Cloud type
data PointCloud = PointCloud { pcVoxels :: [Voxel]
                                  , pcSide :: Int}
  deriving (Show)

--data Slice = Image.ImageRaster | Image.ImageSparse

instance Semigroup PointCloud where
  (PointCloud ps1 l1) <> (PointCloud ps2 l2) = PointCloud (ps1 `union` ps2) (max l1 l2)

instance Monoid PointCloud where
  mempty = PointCloud [] 0

data Axis = X | Y | Z
  deriving (Show)

instance Eq PointCloud where
  (PointCloud voxel_list_1 _) ==
    (PointCloud voxel_list_2 _) = voxel_list_1 == voxel_list_2

addVoxel :: PointCloud -> Voxel -> PointCloud
addVoxel (PointCloud list a) voxel = PointCloud (list ++ [voxel]) a

removeVoxel :: PointCloud -> Voxel -> PointCloud
removeVoxel (PointCloud list a) voxel = PointCloud (delete voxel list) a

slicePointCloud :: Axis -> PointCloud -> (Coordinate, Coordinate) -> Image.ImageSparse
slicePointCloud X (PointCloud l s) (init, end) = Image.ImageSparse (sliceToPixel X $ filter (\(Voxel x _ _) -> x >= init && x < end) l) s
slicePointCloud Y (PointCloud l s) (init, end) = Image.ImageSparse (sliceToPixel Y $ filter (\(Voxel _ y _) -> y >= init && y < end) l) s
slicePointCloud Z (PointCloud l s) (init, end) = Image.ImageSparse (sliceToPixel Z $ filter (\(Voxel _ _ z) -> z >= init && z < end) l) s

sliceToPixel :: Axis -> [Voxel] -> [Image.Pixel]
sliceToPixel _ [] = []
sliceToPixel X (Voxel x y z:vs) = Image.Pixel (y + 1) (z + 1) : sliceToPixel X vs
sliceToPixel Y (Voxel x y z:vs) = Image.Pixel (x + 1) (z + 1) : sliceToPixel Y vs
sliceToPixel Z (Voxel x y z:vs) = Image.Pixel (x + 1) (y + 1) : sliceToPixel Z vs

{- addSliceToPointCloud :: Axis -> Coordinate -> Slice -> PointCloud -> PointCloud
addSliceToPointCloud X c (ImageSparse ps s') (PointCloud vs s) = PointCloud (vs ++ (map (buildVoxel c) ps)) s
addSliceToPointCloud X c (ImageRaster m) (PointCloud ps s) = undefined
  where -}
