module Main where

import CLI

import Codec.PointCloud.Utils
import Codec.PointCloud.Compression.Dyadic
import Codec.PointCloud.Compression.PLY (pc2PLY)
--import Codec.PointCloud.Driver.PLY.Parser (parsePLY, parseVertexPLY)
--import Codec.PointCloud.Driver.PLY.Output (writePLY)
import Codec.PointCloud.Driver.PLY (parsePLY, parsePointCloud, writePLY)

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
      (Right ply) <- pure $ Right . pc2PLY =<< decodeGeometry edx
      let decompressedFileName = filePathFormat ".dec.ply" input
      putStrLn $ "Decoding completed! Writing " ++ decompressedFileName
      --B.writeFile decompressedFileName pc
      writePLY decompressedFileName ply
      exitSuccess
    (Parse input) -> do
      fileContents <- B.readFile input
      parsed <- evaluate $ force $ parsePointCloud fileContents
      putStrLn $ "Parse finished"
    (Error mError) -> do
      putStrLn mError
      exitFailure


mainDecoder = do
  (Right edx) <- readEDX "assets/simple.edx"
  (Right ply) <- pure $ Right . pc2PLY =<< decodeGeometry edx
  return (ply)

mainEncoder = do
  let file = "assets/simple.ply"
  plyData <- B.readFile file
  (Right x) <- pure $ encodeGeometry X =<< parsePLY plyData 
  return x
  --return plyData

  -- (Right edx) <- pure $ buildEDX =<< encodeGeometry X =<< parsePLY plyData
  -- writeEDX "assets/teste.edx" edx
  -- return ()
