module Main where

import Data.Utils -- (getPointCloud)
import Data.Structures.PointCloud
import Data.Input.PLY -- (parsePLY)
import qualified Data.ByteString as B (readFile)
import Data.Structures.Image
import Data.Structures.Tree
import Data.Output.Bitstream

--main :: IO (Either String PointCloud)
main = do
  pcData <- B.readFile "test3.ply"
  let tft = makeTriForce X =<< getPointCloud =<< filterVertex =<< parsePLY' pcData
  case tft of
    (Right tft') -> return $ Right $ fmap triForce2Bin tft'
    (_) -> return $ Left $ "string"
  --return $ getPointCloud =<< filterVertex =<< parsePLY' pcData
  --putStrLn "Hello"
