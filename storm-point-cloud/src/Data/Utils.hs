{-# LANGUAGE OverloadedStrings #-}
module Data.Utils where
  
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Structures.PointCloud
import Data.Input.Types
import Control.Applicative
import Control.Monad (join)
import Data.List (findIndex)
import Data.Either

type Label = ByteString

filterVertex :: PLY -> Either String PLY
filterVertex (PLY h d) = do
                        (before, current, element) <- getUntilLabel (hElems h) "vertex" 0
                        return $ PLY (Header (hFormat h) [element]) $ take current $ drop before d

getUntilLabel :: [Element] -> Label -> Int -> Either String (Int, Int, Element)
getUntilLabel [] l _ = Left $ "Didn't find label " ++ unpack l ++ " in getUntilLabel"
getUntilLabel (e:es) l c
  | elName e == l = Right (c, elNum e, e)
  | otherwise     = getUntilLabel es l (c + elNum e)

getPointCloud :: PLY -> Either String PointCloud
getPointCloud ply = getCoordinatesIndexes (plyHeader ply) >>= extractVoxels (plyData ply)

extractVoxels :: [Values] -> (Int, Int, Int) -> Either String PointCloud
extractVoxels dss (x,y,z) =
  (\v -> PointCloud v (side v) (computeNBits (side v + 1))) <$> sequence ((\ds -> Voxel <$> g (ds !! x) <*> g (ds !! y) <*> g (ds !! z)) <$> dss)
  where g (FloatS n) = Right (round n :: Int)
        g _ = Left "Data is not float"
        side v = computePower2 $ computePCLimit' v

computeNBits :: Int -> Int
computeNBits n = round (logBase 2 (fromIntegral n :: Float)) :: Int

computePower2 :: Int -> Int
computePower2 n = head $ dropWhile (< n) [ 2^i | i <- [0..]]

computePCLimit :: [Voxel] -> Int
computePCLimit v = maximum [maxX - minX, maxY - minY, maxZ - minZ]
  where (maxX, maxY, maxZ) = maxLimit v
        (minX, minY, minZ) = minLimit v

computePCLimit' :: [Voxel] -> Int
computePCLimit' = foldr (max . largestDimension) 0

getCoordinatesIndexes :: Header -> Either String (Int, Int, Int)
getCoordinatesIndexes (Header _ els) = do
                                      props <- findPropsFromLabel els "vertex"
                                      (,,) <$> props `fromLabel` "x" <*> props `fromLabel` "y" <*> props `fromLabel` "z"

findPropsFromLabel :: [Element] -> Label -> Either String [Property]
findPropsFromLabel [] l = Left $ "Didn't find label " ++ unpack l ++ " in findPropsFromLabel"
findPropsFromLabel (e:es) l
  | elName e == l = Right $ elProps e
  | otherwise     = findPropsFromLabel es l

fromLabel :: [Property] -> Label -> Either String Int
fromLabel ps axis = maybe (Left errorMsg) Right mIndex
  where mIndex = findIndex (\p -> sPropName p == axis) ps
        errorMsg = "Didn't find label " ++ unpack axis
