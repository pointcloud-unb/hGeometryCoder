module Dyadic where

import Data.Structures.PointCloud
import Data.Structures.Tree as T
import Data.Structures.Image as I
import Data.Input.Types
import Data.Utils
import Bitstream
import Data.Matrix
import Data.Maybe

encodeGeometry :: Axis -> PLY -> Either String (Bin, Int)
encodeGeometry axis ply = do
                        (tft, side) <- buildTriForce axis ply
                        Right (writeRootSilhoutte tft ++ concatMap triForce2Bin tft, side)

buildTriForce :: Axis -> PLY -> Either String (T.BinTree (T.BinTree I.ImageSparse), Int)
buildTriForce axis ply = pc2TriForce axis =<< getPointCloud =<< filterVertex ply

writeRootSilhoutte :: T.BinTree (T.BinTree I.ImageSparse) -> Bin
writeRootSilhoutte tft = iRaster2Bin $ I.sparseToRaster $ getRoot tft
    where getRoot tft = T.getValue $ T.getValue tft

iRaster2Bin :: I.ImageRaster -> Bin
iRaster2Bin = map f . toList
    where f b = if b then 1 else 0

iRaster2Bin' :: Matrix (Maybe Bool) -> Bin
iRaster2Bin' m = map f $ filter isJust $ toList m
    where f (Just a) = if a then 1 else 0

triForce2Bin :: T.BinTree I.ImageSparse -> Bin
triForce2Bin tft = iRaster2Bin' (applyMask y yL) ++ iRaster2Bin' (applyMask yL yR)
    where y = I.sparseToRaster $ T.getValue tft
          yL = I.sparseToRaster $ T.getValue $ T.left tft
          yR = I.sparseToRaster $ T.getValue $ T.right tft

applyMask :: I.ImageRaster -> I.ImageRaster -> Matrix (Maybe Bool)
applyMask = elementwise f
    where f v1 v2
            | not v1        = Nothing
            | otherwise     = Just v2
