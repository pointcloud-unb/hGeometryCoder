module Data.Structures.Voxel where
import Data.Utils

-- Voxel type
data Voxel = Voxel { getU :: Coordinate
                   , getV :: Coordinate
                   , getW :: Coordinate }
  deriving(Show)


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

maxLimit :: [Voxel] -> (Int, Int, Int)
maxLimit v = (maximum $ map getU v, maximum $ map getV v, maximum $ map getW v)

minLimit :: [Voxel] -> (Int, Int, Int)
minLimit v = (minimum $ map getU v, minimum $ map getV v, minimum $ map getW v)

largestDimension :: Voxel -> Int
largestDimension (Voxel u v w) = maximum [u,v,w]