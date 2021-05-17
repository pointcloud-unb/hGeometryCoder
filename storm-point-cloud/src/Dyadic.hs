{-# LANGUAGE OverloadedStrings #-}
module Dyadic where

import qualified Data.ByteString as B
import Data.Structures.PointCloud
import Data.Structures.Voxel
import Data.Structures.Tree
import Data.Structures.Image
import Data.Input.Types
import Data.Utils
import Bitstream
import Data.Matrix
import Data.Maybe
import Data.Input.DecoderParser
import Data.Structures.PCBitStream
import qualified Data.Set as S
import qualified Data.Matrix as M

-- Encoder

encodeGeometry :: Axis -> PLY -> Either String (Bin, PointCloudSize, Axis)
encodeGeometry axis ply = do
                        (tft, size) <- buildTriForce axis ply
                        Right (writeRootSilhoutte tft ++ concatMap triForce2Bin tft, size, axis)

buildTriForce :: Axis -> PLY -> Either String (TriForceTree, PointCloudSize)
buildTriForce axis ply = pc2TriForce axis =<< getPointCloud =<< filterFromLabel "vertex" ply

writeRootSilhoutte :: TriForceTree -> Bin
writeRootSilhoutte tft = iRaster2Bin $ sparseToRaster $ getRoot tft
    where getRoot tft = getValue $ getValue tft

iRaster2Bin :: ImageRaster -> Bin
iRaster2Bin = map f . toList
    where f b = if b then 1 else 0

iRaster2Bin' :: Matrix (Maybe Bool) -> Bin
iRaster2Bin' m = map f $ filter isJust $ toList m
    where f (Just a) = if a then 1 else 0

triForce2Bin :: TriForce -> Bin
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

decodeGeometry :: B.ByteString -> Either String Bin
decodeGeometry b = do
    let resultDecodeParser = bitstreamPC b
    case resultDecodeParser of
        Nothing -> Left "Error reading .edx file!"
        Just (PCBitStream padding axis size payload) -> do
                                        let imageBin = extractData payload padding
                                        let rootSilhoutte = buildRootSilhoutte (take (size*size) imageBin) size
                                        Left "string"

buildRootSilhoutte :: Bin -> PointCloudSize -> ImageSparse
buildRootSilhoutte b s = rasterToSparse $ f <$> M.fromList s s b
    where f bit = bit == 1

bString2Bin :: B.ByteString -> Bin
bString2Bin b = concatMap integral2BitList $ B.unpack b

extractData :: B.ByteString -> Padding -> Bin
extractData b p = take (length bin - fromIntegral p) bin
    where bin = bString2Bin b