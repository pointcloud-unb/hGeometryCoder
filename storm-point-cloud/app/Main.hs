module Main where

import Data.Utils -- (getPointCloud)
import Data.Structures.PointCloud (PointCloud)
import Data.Input.PLY -- (parsePLY)
import qualified Data.ByteString as B (readFile)
import Control.Monad (join)


--main :: IO (Either String PointCloud)
main = do
  --pcData <- B.readFile "test2.ply"
  --return $ getPointCloud =<< parsePLY' pcData
  putStrLn "Hello"
