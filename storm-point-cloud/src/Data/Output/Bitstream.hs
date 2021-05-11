{-# LANGUAGE OverloadedStrings #-}
module Data.Output.Bitstream where
import qualified Data.Structures.Tree as T
import qualified Data.Structures.Image as I
import Data.Matrix

--writeHeader :: (T.BinTree (T.BinTree I.ImageSparse)) -> Header

writeRootSilhoutte :: T.BinTree (T.BinTree I.ImageSparse) -> [Int]
writeRootSilhoutte tft = iRaster2Bin $ I.sparseToRaster $ getRoot tft
    where getRoot tft = T.getValue $ T.getValue tft

iRaster2Bin :: I.ImageRaster -> [Int]
iRaster2Bin = map f . toList
    where f b = if b then 1 else 0

iRaster2Bin' :: Matrix (Maybe Bool) -> [Int]
iRaster2Bin' m = filter (>=0) $ toList $ fmap f m
    where f (Just a) = if a then 1 else 0
          f Nothing = -1

triForce2Bin :: T.BinTree I.ImageSparse -> [Int]
triForce2Bin tft = iRaster2Bin' (applyMask y yL) ++ iRaster2Bin' (applyMask yL yR)
    where y = I.sparseToRaster $ T.getValue tft
          yL = I.sparseToRaster $ T.getValue $ T.left tft
          yR = I.sparseToRaster $ T.getValue $ T.right tft

applyMask :: I.ImageRaster -> I.ImageRaster -> Matrix (Maybe Bool)
applyMask = elementwise f
    where f v1 v2
            | not v1        = Nothing
            | v2            = Just True
            | otherwise     = Just False
