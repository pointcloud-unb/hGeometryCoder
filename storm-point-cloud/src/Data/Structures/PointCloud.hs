module Data.Structures.PointCloud where

import Data.List
import qualified Data.Structures.Image as I

-- Voxel type
type Coordinate = Int
data Voxel = Voxel { getX :: Coordinate
                   , getY :: Coordinate
                   , getZ :: Coordinate }
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

-- getX :: Voxel -> Coordinate
-- getX (Voxel x y z) = x

-- getY :: Voxel -> Coordinate
-- getY (Voxel x y z) = y

-- getZ :: Voxel -> Coordinate
-- getZ (Voxel x y z) = z

maxLimit :: [Voxel] -> (Int, Int, Int)
maxLimit v = (maximum $ map getX v, maximum $ map getY v, maximum $ map getZ v)

minLimit :: [Voxel] -> (Int, Int, Int)
minLimit v = (minimum $ map getX v, minimum $ map getY v, minimum $ map getZ v)

largestDimension :: Voxel -> Int
largestDimension (Voxel x y z) = maximum [x,y,z]

-- Point Cloud type
data PointCloud = PointCloud { pcVoxels :: [Voxel]
                             , pcSide   :: Int
                             , nBits    :: Int}
  deriving (Show)

--data Slice = Image.ImageRaster | Image.ImageSparse

instance Semigroup PointCloud where
  (PointCloud ps1 l1 b1) <> (PointCloud ps2 l2 b2) = PointCloud (ps1 `union` ps2) (max l1 l2) (max b1 b2)

instance Monoid PointCloud where
  mempty = PointCloud [] 0 0

data Axis = X | Y | Z
  deriving (Show)

instance Eq PointCloud where
  (PointCloud voxel_list_1 _ _) ==
    (PointCloud voxel_list_2 _ _) = voxel_list_1 == voxel_list_2

addVoxel :: PointCloud -> Voxel -> PointCloud
addVoxel (PointCloud list a b) voxel = PointCloud (list ++ [voxel]) a b

removeVoxel :: PointCloud -> Voxel -> PointCloud
removeVoxel (PointCloud list a b) voxel = PointCloud (delete voxel list) a b

slicePointCloud :: Axis -> Coordinate -> PointCloud -> (PointCloud, PointCloud)
slicePointCloud axis cutIndex (PointCloud v s b) =
  (PointCloud (filter (f (<=) axis cutIndex) v) s b,
   PointCloud (filter (f (>) axis cutIndex) v) s b)
  where
    f :: (Coordinate -> Coordinate -> Bool) -> Axis -> Coordinate -> Voxel -> Bool
    f op X cutIndex v = (getX v) `op` cutIndex
    f op Y cutIndex v = (getY v) `op` cutIndex
    f op Z cutIndex v = (getZ v) `op` cutIndex


type Range = (Int, Int)

slicePointCloud' :: Axis -> Range -> PointCloud -> PointCloud
slicePointCloud' axis r (PointCloud v s b) =
  PointCloud (filter (f axis r) v) s b
  where
    f :: Axis -> Range -> Voxel -> Bool
    f X (a,b) v = (getX v) >= a && (getX v) <= b
    f Y (a,b) v = (getY v) >= a && (getY v) <= b
    f Z (a,b) v = (getZ v) >= a && (getZ v) <= b


sliceToSilhoutte :: Axis -> PointCloud -> I.ImageSparse
sliceToSilhoutte a (PointCloud v s _) = I.ImageSparse (sliceToPixelList a v) s



--g :: Axis -> PointCloud -> Coordinate -> I.ImageSparse

sliceToPixelList :: Axis -> [Voxel] -> [I.Pixel]
sliceToPixelList _ [] = []
sliceToPixelList X (Voxel _ y z : vs) = I.Pixel (y + 1) (z + 1) : sliceToPixelList X vs
sliceToPixelList Y (Voxel x y z : vs) = I.Pixel (x + 1) (z + 1) : sliceToPixelList Y vs
sliceToPixelList Z (Voxel x y z : vs) = I.Pixel (x + 1) (y + 1) : sliceToPixelList Z vs

{- addSliceToPointCloud :: Axis -> Coordinate -> Slice -> PointCloud -> PointCloud
addSliceToPointCloud X c (ImageSparse ps s') (PointCloud vs s) = PointCloud (vs ++ (map (buildVoxel c) ps)) s
addSliceToPointCloud X c (ImageRaster m) (PointCloud ps s) = undefined
  where -}
