module Main where

import Data.Utils -- (getPointCloud)
import Data.Structures.PointCloud (PointCloud)
import Data.Input.PLY -- (parsePLY)
import qualified Data.ByteString as B (readFile)
import Control.Monad (join)

--main :: IO (Either String PointCloud)
main = do
  pcData <- B.readFile "test3.ply"
  --return $ parsePLY' pcData
  --return $ filterVertex =<< parsePLY' pcData
  return $ getPointCloud =<< filterVertex =<< parsePLY' pcData
  --putStrLn "Hello"
