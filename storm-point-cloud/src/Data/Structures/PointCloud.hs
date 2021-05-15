module Data.Structures.PointCloud where

import Data.List
import Data.Structures.Image
import Data.Structures.Pixel
import qualified Data.Set as S
import Data.Utils
import Data.Input.Types
import Data.Structures.Voxel

-- Point Cloud type
data PointCloud = PointCloud { pcVoxels :: S.Set Voxel
                             , pcSize   :: Int}
  deriving (Show)

instance Semigroup PointCloud where
  (PointCloud ps1 l1) <> (PointCloud ps2 l2) = PointCloud (ps1 `S.union` ps2) (max l1 l2)

instance Monoid PointCloud where
  mempty = PointCloud S.empty 0 

instance Eq PointCloud where
  (PointCloud voxel_set_1 size1) ==
    (PointCloud voxel_set_2 size2) = voxel_set_1 == voxel_set_2 && size1 == size2

addVoxel :: PointCloud -> Voxel -> PointCloud
addVoxel (PointCloud set size) voxel = PointCloud (S.insert voxel set) size

removeVoxel :: PointCloud -> Voxel -> PointCloud
removeVoxel (PointCloud set size) voxel = PointCloud (S.delete voxel set) size

getPointCloud :: PLY -> Either String PointCloud
getPointCloud ply = getCoordinatesIndexes (plyHeader ply) >>= extractVoxels (plyData ply)

extractVoxels :: [Values] -> (Coordinate, Coordinate, Coordinate) -> Either String PointCloud
extractVoxels dss (x,y,z) =
  (\v -> PointCloud (S.fromList v) (side v)) <$> sequence ((\ds -> Voxel <$> g (ds !! x) <*> g (ds !! y) <*> g (ds !! z)) <$> dss)
  where g (FloatS n) = Right (round n :: Int)
        g _ = Left "Data is not float"
        side v = computePower2 $ computePCLimit' v

computePCLimit :: [Voxel] -> PointCloudSize
computePCLimit v = maximum [maxX - minX, maxY - minY, maxZ - minZ]
  where (maxX, maxY, maxZ) = maxLimit v
        (minX, minY, minZ) = minLimit v

computePCLimit' :: [Voxel] -> PointCloudSize
computePCLimit' = foldr (max . largestDimension) 0

slicePointCloud :: Axis -> Coordinate -> PointCloud -> (PointCloud, PointCloud)
slicePointCloud axis cutIndex (PointCloud v s) =
  (PointCloud (S.filter (f (<=) axis cutIndex) v) s,
   PointCloud (S.filter (f (>) axis cutIndex) v) s)
  where
    f :: (Coordinate -> Coordinate -> Bool) -> Axis -> Coordinate -> Voxel -> Bool
    f op X cutIndex v = getX v `op` cutIndex
    f op Y cutIndex v = getY v `op` cutIndex
    f op Z cutIndex v = getZ v `op` cutIndex

slicePointCloud' :: Axis -> Range -> PointCloud -> PointCloud
slicePointCloud' axis r (PointCloud v s) =
  PointCloud (S.filter (f axis r) v) s
  where
    f :: Axis -> Range -> Voxel -> Bool
    f X (a,b) v = getX v >= a && getX v <= b
    f Y (a,b) v = getY v >= a && getY v <= b
    f Z (a,b) v = getZ v >= a && getZ v <= b

sliceToSilhoutte :: Axis -> PointCloud -> ImageSparse
sliceToSilhoutte a (PointCloud v s) = ImageSparse (sliceToPixelList a (S.toList v)) s

sliceToPixelList :: Axis -> [Voxel] -> S.Set Pixel
sliceToPixelList _ [] = S.empty
sliceToPixelList X (Voxel _ y z : vs) = S.fromList [Pixel (y + 1) (z + 1)] `S.union` sliceToPixelList X vs
sliceToPixelList Y (Voxel x y z : vs) = S.fromList [Pixel (x + 1) (z + 1)] `S.union` sliceToPixelList Y vs
sliceToPixelList Z (Voxel x y z : vs) = S.fromList [Pixel (x + 1) (y + 1)] `S.union` sliceToPixelList Z vs
