{-# LANGUAGE OverloadedStrings #-}
module Data.Utils
  ( getPointCloud
  ) where
import Data.ByteString.Char8 (ByteString, unpack)
import Data.PLY.PointCloud
import Data.PLY.Types
import Control.Applicative
import Data.List (findIndex)

getPointCloud :: PLY -> Either String PointCloud
getPointCloud ply = getCoordinatesIndexes (plyHeader ply) >>= extractVoxels (plyData ply)


extractVoxels :: [Values] -> (Int, Int, Int) -> Either String PointCloud
extractVoxels dss (x,y,z) =
  PointCloud <$> (sequence $ (\ds -> Voxel <$>
                                (g (ds !! x)) <*>
                                (g (ds !! y)) <*>
                                (g (ds !! z))
                              ) <$> dss)
  where g (FloatS n) = Right n
        g _ = Left "Data is not float"

getCoordinatesIndexes :: Header -> Either String (Int, Int, Int)
getCoordinatesIndexes (Header _ []) = Left "Header without elements!"
getCoordinatesIndexes (Header _ ((Element _ _ ps):_)) =
  (,,) <$> ps `fromLabel` "x" <*> ps `fromLabel` "y" <*> ps `fromLabel` "z"

getCoordinatesIndexes (Header _ els) =
  where
    x = fmap (\ps -> (,,) <$> ps `fromLabel` "x" <*> ps `fromLabel` "y" <*> ps `fromLabel` "z") pss
    pss = elProps <$> els 


fromLabel :: [Property] -> ByteString -> Either String Int
fromLabel ps axis = maybe (Left errorMsg) Right mIndex
  where mIndex = findIndex (\p -> sPropName p == axis) ps
        errorMsg = "Didn't find label " ++ (unpack axis)



            
