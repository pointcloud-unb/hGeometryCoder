{-# LANGUAGE OverloadedStrings #-}

module Codec.PointCloud.Compression.Dyadic where

import Codec.PointCloud.Types.PointCloud
import Codec.PointCloud.Types.Voxel
import Codec.PointCloud.Types.Tree
import Codec.PointCloud.Types.Image
import Codec.PointCloud.Driver.PLY.Types
import Codec.PointCloud.Driver.EDX.Bitstream
import Codec.PointCloud.Driver.EDX.Types
import Codec.PointCloud.Utils
import Codec.PointCloud.Driver.PLY.Utils


import Data.Matrix
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Matrix as M
import qualified Data.ByteString.Lazy as B

-- Encoder

encodeGeometry :: Axis -> PLY -> Either String (Bin, PointCloudSize, Axis)
encodeGeometry axis ply = do
                        (tft, size) <- buildTriForce axis ply
                        Right (writeRootSilhoutte tft ++ concatMap triForce2Bin tft, size, axis)

buildTriForce :: Axis -> PLY -> Either String (ISparseTriForceTree, PointCloudSize)
buildTriForce axis ply = pc2TriForce axis =<< getPointCloud axis =<< filterFromLabel "vertex" ply

writeRootSilhoutte :: ISparseTriForceTree -> Bin
writeRootSilhoutte tft = iRaster2Bin $ sparseToRaster $ getRoot tft
    where getRoot tft = getValue $ getValue tft

iRaster2Bin :: ImageRaster -> Bin
iRaster2Bin = map f . toList
    where f b = if b then 1 else 0

iRaster2Bin' :: Matrix (Maybe Bool) -> Bin
iRaster2Bin' m = map f $ filter isJust $ toList m
    where f (Just a) = if a then 1 else 0

triForce2Bin :: ISparseTriForce -> Bin
triForce2Bin tft = iRaster2Bin' (applyMaskEncoder y yL) ++ iRaster2Bin' (applyMaskEncoder yL yR)
    where y = sparseToRaster $ getValue tft
          yL = sparseToRaster $ getValue $ left tft
          yR = sparseToRaster $ getValue $ right tft

applyMaskEncoder :: ImageRaster -> ImageRaster -> Matrix (Maybe Bool)
applyMaskEncoder = elementwise f
    where f v1 v2
            | not v1        = Nothing
            | otherwise     = Just v2

-- Decoder

decodeGeometry :: EDX -> Either String PointCloud
decodeGeometry (EDX (EDXHeader _ axis _ bitSize _) payload) = do
                                        let size = 2^ fromIntegral bitSize
                                        let (root, rest) = extractData payload size
                                        let rootSilhoutte =  map (== 1) root
                                        let tfr = triForceTreeRange (0, size - 1)
                                        let (pc, _, b) = triForceR2PC mempty tfr rootSilhoutte rest (axis, size)
                                        Right pc

buildRootSilhoutte :: Bin -> PointCloudSize -> ImageSparse
buildRootSilhoutte b s = rasterToSparse $ f <$> M.fromList s s b
    where f bit = bit == 1

bString2Bin :: B.ByteString -> Bin
bString2Bin b = concatMap integral2BitList $ B.unpack b

extractData :: B.ByteString -> PointCloudSize -> (Bin, Bin)
extractData b s = splitAt (s*s) (bString2Bin b)
