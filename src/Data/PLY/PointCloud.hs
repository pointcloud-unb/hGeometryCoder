module Data.PLY.PointCloud where

data Voxel = Voxel Int Int Int
  deriving(Show)

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