module Data.PLY.PointCloud where

import      Data.List

-- Voxel type
data Voxel = Voxel Int Int Int
  deriving(Show)

instance Eq Voxel where
  -- (==) Voxel -> Voxel -> Bool
  (Voxel x1 y1 z1) == 
    (Voxel x2 y2 z2) = (x1 == x2) && (y1 == y2) && (z1 == z2)

  -- (!=) Voxel -> Voxel -> Bool
  (Voxel x1 y1 z1) /= 
    (Voxel x2 y2 z2) = (x1 /= x2) || (y1 /= y2) || (z1 /= z2)

-- Less comparison
less :: Voxel -> Voxel -> Bool
(Voxel x1 y1 z1) `less` (Voxel x2 y2 z2) 
  = if x1 == x2 then
      if y1 == y2 then
        z1 < z2
      else
        y1 < y2
    else
      x1 < x2
  
-- Less or Equal comparison
lessOrEqual :: Voxel -> Voxel -> Bool
(Voxel x1 y1 z1) `lessOrEqual` (Voxel x2 y2 z2) 
  = if x1 == x2 then
      if y1 == y2 then
        z1 <= z2
      else
        y1 <= y2
    else
      x1 <= x2

-- Greater comparison
greater :: Voxel -> Voxel -> Bool
(Voxel x1 y1 z1) `greater` (Voxel x2 y2 z2) 
  = if x1 == x2 then
      if y1 == y2 then
        z1 > z2
      else
        y1 > y2
    else
      x1 > x2
  
-- greater or Equal comparison
greaterOrEqual :: Voxel -> Voxel -> Bool
(Voxel x1 y1 z1) `greaterOrEqual` (Voxel x2 y2 z2) 
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