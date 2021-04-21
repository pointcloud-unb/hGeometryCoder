module Data.PLY.PointCloud where

import      Data.List

-- Voxel type
type Coordinate = Float
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
newtype PointCloud = PointCloud [Voxel]
  deriving(Show)

instance Eq PointCloud where
  (PointCloud voxel_list_1) ==
    (PointCloud voxel_list_2) = voxel_list_1 == voxel_list_2

addVoxel :: PointCloud -> Voxel -> PointCloud
addVoxel (PointCloud list) voxel = PointCloud (list ++ [voxel]) 

removeVoxel :: PointCloud -> Voxel -> PointCloud
removeVoxel (PointCloud list) voxel = PointCloud (delete voxel list)