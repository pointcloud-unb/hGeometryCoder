{-# LANGUAGE OverloadedStrings #-}
module Data.Utils
  (getVoxels
  ,getPointCloud
  ) where
import Data.ByteString (ByteString)
import Data.PLY.PointCloud
import Data.PLY.Types

getPointCloud :: Either String PLY -> Either String PointCloud
getPointCloud (Left msg) = Left msg
getPointCloud (Right p) = getVoxels p

getVoxels :: PLY -> Either String PointCloud
getVoxels (PLY h d)= getCoordinatesIndexes h >>= \t -> Right $ PointCloud (extractVoxels t d)

extractVoxels :: (Int, Int, Int) -> [Values] -> [Voxel]
extractVoxels (_,_,_) [] = []
extractVoxels (x,y,z) (d:ds) = Voxel (g (d !! x)) (g (d !! y)) (g (d !! z)) : extractVoxels (x,y,z) ds
    where g (FloatS n) = n
          g _ = -1.0

getCoordinatesIndexes :: Header -> Either String (Int, Int, Int)
getCoordinatesIndexes (Header _ []) = Left "Header without elements!"
getCoordinatesIndexes (Header _ ((Element _ _ p):_)) = result (getC p "x" 0,  getC p "y" 0, getC p "z" 0)
    where result (x, y, z) 
            | x == -1 || y == -1 || z == -1 = Left "Header without XYZ coordinates!"
            | otherwise                     = Right (x, y, z)

getC :: [Property] -> ByteString -> Int -> Int
getC [] _ _ = -1
getC (p:ps) c n
    | sPropName p == c  = n
    | otherwise         = getC ps c (n+1)


            