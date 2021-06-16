{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module Codec.PointCloud.Types.PointCloud where

import Codec.PointCloud.Types.Image
import Codec.PointCloud.Types.Pixel
import Codec.PointCloud.Types.Voxel
import Codec.PointCloud.Driver.PLY.Types
import Codec.PointCloud.Utils
import Codec.PointCloud.Driver.PLY.Utils

import qualified Data.Set as S
import Data.List
import Data.Matrix
import Data.Maybe

import Flat
import GHC.Generics (Generic)
import Control.DeepSeq


--class Cloud where
  --empty :: PointCloudSize -> PointCloud
  --null :: PointCloud -> Bool
  --addVoxel :: Voxel -> PointCloud -> PointCloud
  --removeVoxel :: Voxel -> PointCloud -> PointCloud

--class Sliceable where
  --slice :: Axis -> Range -> PointCloud -> PointCloud

--class Explodable where
  --explode :: PointCloud -> [PointCloud]

--class Lightable where
  --silhouette :: (Image a) => Axis -> PointCloud -> a


-- Point Cloud type
data PointCloud = PointCloud { pcVoxels :: S.Set Voxel
                             , pcSize   :: Int}
  deriving (Show, Generic, Flat, NFData)

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

getPointCloud :: Axis -> PLY -> Either String PointCloud
getPointCloud axis ply = getCoordinatesIndexes (plyHeader ply) >>= extractVoxels axis (plyData ply)

getVoxelsLength :: PointCloud -> Int
getVoxelsLength (PointCloud voxelSet _) = S.size voxelSet

extractVoxels :: Axis -> [DataLine] -> (Coordinate, Coordinate, Coordinate) -> Either String PointCloud
extractVoxels axis dss (u,v,w) =
  (\x -> PointCloud (S.fromList x) (side x)) <$> sequence (createVoxel <$> dss)
  where createVoxel ds = reordenate axis <$> (Voxel <$> g (ds !! u) <*> g (ds !! v) <*> g (ds !! w))
        g (FloatS n) = Right (round n :: Int)
        g _ = Left "Data is not float"
        side x = computePower2 $ computePCLimit' x

reordenate :: Axis -> Voxel -> Voxel
reordenate X (Voxel u v w) = Voxel u v w
reordenate Y (Voxel u v w) = Voxel v w u
reordenate Z (Voxel u v w) = Voxel w u v

-- computePCLimit :: [Voxel] -> PointCloudSize
-- computePCLimit v = maximum [maxU - minU, maxV - minV, maxW - minW]
--   where (maxU, maxV, maxW) = maxLimit v
--         (minU, minV, minW) = minLimit v

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
sliceToPixelList Y (Voxel u _ w : vs) = S.fromList [Pixel (w + 1) (u + 1)] `S.union` sliceToPixelList Y vs
sliceToPixelList Z (Voxel u v _ : vs) = S.fromList [Pixel (u + 1) (v + 1)] `S.union` sliceToPixelList Z vs

addRasterToPointCloud :: Range -> Axis -> ImageRaster -> PointCloud -> PointCloud
addRasterToPointCloud (coordinate,_) axis iR pc = pc <> PointCloud voxels (nrows iR)
    where voxels = S.fromList $ catMaybes (toList $ mapPos (\p a -> f p axis) iR)
          f (lin, col) ax
            | not (getElem lin col iR)  = Nothing
            | ax == X               = Just $ Voxel coordinate (lin - 1)   (col - 1)
            | ax == Y               = Just $ Voxel (lin - 1)  (col - 1)   coordinate
            | otherwise             = Just $ Voxel (col - 1)  coordinate  (lin - 1)
