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
extractVoxels dss (u,v,w) =
  (\x -> PointCloud (S.fromList x) (side x)) <$> sequence ((\ds -> Voxel <$> g (ds !! u) <*> g (ds !! v) <*> g (ds !! w)) <$> dss)
  where g (FloatS n) = Right (round n :: Int)
        g _ = Left "Data is not float"
        side x = computePower2 $ computePCLimit' x

computePCLimit :: [Voxel] -> PointCloudSize
computePCLimit v = maximum [maxU - minU, maxV - minV, maxW - minW]
  where (maxU, maxV, maxW) = maxLimit v
        (minU, minV, minW) = minLimit v

computePCLimit' :: [Voxel] -> PointCloudSize
computePCLimit' = foldr (max . largestDimension) 0

slicePointCloud :: Axis -> Coordinate -> PointCloud -> (PointCloud, PointCloud)
slicePointCloud axis cutIndex (PointCloud v s) =
  (PointCloud (S.filter (f (<=) axis cutIndex) v) s,
   PointCloud (S.filter (f (>) axis cutIndex) v) s)
  where
    f :: (Coordinate -> Coordinate -> Bool) -> Axis -> Coordinate -> Voxel -> Bool
    f op X cutIndex v = getU v `op` cutIndex
    f op Y cutIndex v = getV v `op` cutIndex
    f op Z cutIndex v = getW v `op` cutIndex

slicePointCloud' :: Axis -> Range -> PointCloud -> PointCloud
slicePointCloud' axis r (PointCloud v s) =
  PointCloud (S.filter (f axis r) v) s
  where
    f :: Axis -> Range -> Voxel -> Bool
    f X (a,b) v = getU v >= a && getU v <= b
    f Y (a,b) v = getV v >= a && getV v <= b
    f Z (a,b) v = getW v >= a && getW v <= b

sliceToSilhoutte :: Axis -> PointCloud -> ImageSparse
sliceToSilhoutte a (PointCloud v s) = ImageSparse (sliceToPixelList a (S.toList v)) s

sliceToPixelList :: Axis -> [Voxel] -> S.Set Pixel
sliceToPixelList _ [] = S.empty
sliceToPixelList X (Voxel _ v w : vs) = S.fromList [Pixel (v + 1) (w + 1)] `S.union` sliceToPixelList X vs
sliceToPixelList Y (Voxel u _ w : vs) = S.fromList [Pixel (u + 1) (w + 1)] `S.union` sliceToPixelList Y vs
sliceToPixelList Z (Voxel u v _ : vs) = S.fromList [Pixel (u + 1) (v + 1)] `S.union` sliceToPixelList Z vs
