{-# LANGUAGE OverloadedStrings #-}
module Dyadic where

import Data.Structures.PointCloud
import Data.Structures.Tree
import Data.Structures.Image
import Data.Input.Types
import Data.Utils
import Bitstream
import Data.Matrix
import Data.Maybe

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
triForce2Bin tft = iRaster2Bin' (applyMask y yL) ++ iRaster2Bin' (applyMask yL yR)
    where y = sparseToRaster $ getValue tft
          yL = sparseToRaster $ getValue $ left tft
          yR = sparseToRaster $ getValue $ right tft

applyMask :: ImageRaster -> ImageRaster -> Matrix (Maybe Bool)
applyMask = elementwise f
    where f v1 v2
            | not v1        = Nothing
            | otherwise     = Just v2
