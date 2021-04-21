module Main (main) where

import Data.Utils (getPointCloud)
import Data.PLY.PointCloud (PointCloud)
import Data.PLY (loadPLY)


main :: IO (Either String PointCloud)
main = getPointCloud <$> loadPLY "test.ply"
