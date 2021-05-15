module Data.Structures.Voxel where
import Data.Utils

-- Voxel type
data Voxel = Voxel { getX :: Coordinate
                   , getY :: Coordinate
                   , getZ :: Coordinate }
  deriving(Show)


instance Eq Voxel where
  -- (==) Voxel -> Voxel -> Bool
  (Voxel x1 y1 z1) ==
    (Voxel x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

  -- (!=) Voxel -> Voxel -> Bool
  (Voxel x1 y1 z1) /=
    (Voxel x2 y2 z2) = x1 /= x2 || y1 /= y2 || z1 /= z2

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

maxLimit :: [Voxel] -> (Int, Int, Int)
maxLimit v = (maximum $ map getX v, maximum $ map getY v, maximum $ map getZ v)

minLimit :: [Voxel] -> (Int, Int, Int)
minLimit v = (minimum $ map getX v, minimum $ map getY v, minimum $ map getZ v)

largestDimension :: Voxel -> Int
largestDimension (Voxel x y z) = maximum [x,y,z]