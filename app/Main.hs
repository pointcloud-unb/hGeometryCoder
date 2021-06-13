module Main where

import CLI

import Codec.PointCloud.Utils
import Codec.PointCloud.Compression.Dyadic
import Codec.PointCloud.Driver.PLY.PLY
import Codec.PointCloud.Driver.PLY.Parser (parsePLY1, parsePLY')
import Codec.PointCloud.Driver.Bitstream


import Codec.PointCloud.Driver.EDX.Bitstream
import qualified Data.ByteString as B (readFile, writeFile, pack)
import qualified Data.ByteString.Lazy as BL (writeFile)
import System.Exit (exitSuccess, exitFailure)
import System.Environment
import Control.DeepSeq (force)
import Control.Exception (evaluate)


main :: IO ()
main = do
  options <- parseArgs <$> getArgs
  case options of
    (Encode input axis) -> do
      putStrLn $ "Parsing " ++ input
      plyData <- B.readFile input
      putStrLn $ "Encoding " ++ input
--      (Right encodedBin) <- pure $ buildEDX =<< encodeGeometry axis =<< parsePLY plyData
      (Right edx) <- pure $ buildEDX =<< encodeGeometry axis =<< parsePLY plyData
      let compressedFileName = filePathFormat ".edx" input
      putStrLn $ "Encoding completed! Writing " ++ compressedFileName
--      BL.writeFile compressedFileName (B.pack encodedBin)
      writeEDX compressedFileName edx
      exitSuccess
    (Decode input) -> do
      putStrLn $ "Decoding " ++ input
      (Right edx) <- readEDX input
      (Right pc) <- pure $ buildPLY =<< decodeGeometry edx
      let decompressedFileName = filePathFormat ".dec.ply" input
      putStrLn $ "Decoding completed! Writing " ++ decompressedFileName
      B.writeFile decompressedFileName pc
      exitSuccess
    (Parse input) -> do
      fileContents <- B.readFile input
      parsed <- evaluate $ force $ parsePLY1 fileContents
      putStrLn $ "Parse finished"
    (Error mError) -> do
      putStrLn mError
      exitFailure


mainDecoder = do
  (Right edx) <- readEDX "assets/teste.edx"
  (Right pc) <- pure $ buildPLY =<< decodeGeometry edx
  B.writeFile "assets/testeR.ply" pc
  return ()

mainEncoder = do
  let file = "assets/simple.ply"
  plyData <- B.readFile file
  (Right x) <- pure $ parsePLY plyData
  --return x
  --return plyData
  (Right edx) <- pure $ buildEDX =<< encodeGeometry X =<< parsePLY plyData
  writeEDX "assets/teste.edx" edx
  return ()
