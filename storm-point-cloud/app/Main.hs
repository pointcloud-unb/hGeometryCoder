module Main where

import Data.Utils -- (getPointCloud)
import Dyadic
import Data.Input.PLY -- (parsePLY)
import qualified Data.ByteString as B (readFile, writeFile, pack)
import Bitstream
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
  pcData <- B.readFile "test3.ply"
  putStrLn $ "Encoding..."
  (Right encodedBin) <- pure $ buildEDX =<< encodeGeometry X =<< parsePLY' pcData
  putStrLn $ "Encoding completed!"
  B.writeFile "test.edx" (B.pack encodedBin)
  return $ ()
