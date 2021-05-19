module Main where

import Data.Utils
import Dyadic
import Data.Input.PLY
import qualified Data.ByteString as B (readFile, writeFile, pack)
import Bitstream
import System.Exit (exitSuccess, exitFailure)
import System.Environment

type Format = String
data Args = Encode { inputPath :: FilePath
                    , axis :: Axis}
            | Decode { inputPath :: FilePath}
            | Error {errorMessage :: String}

main :: IO ()
main = do
  options <- parseArgs <$> getArgs
  case options of
    (Encode input axis) -> do
      putStrLn $ "Parsing " ++ input
      plyData <- B.readFile input
      putStrLn $ "Encoding " ++ input
      (Right encodedBin) <- pure $ buildEDX =<< encodeGeometry axis =<< parsePLY plyData
      let compressedFileName = filePathFormat ".edx" input
      putStrLn $ "Encoding completed! Writing " ++ compressedFileName
      B.writeFile compressedFileName (B.pack encodedBin)
      exitSuccess
    (Decode input) -> do
      putStrLn $ "Decoding " ++ input
      fileContents <- B.readFile input
      (Right pc) <- pure $ buildPLY =<< decodeGeometry fileContents
      let decompressedFileName = filePathFormat ".ply" input
      putStrLn $ "Decoding completed! Writing " ++ decompressedFileName
      B.writeFile decompressedFileName pc
      exitSuccess
    (Error mError) -> do
      putStrLn mError
      exitFailure

filePathFormat :: FilePath -> FilePath -> FilePath
filePathFormat format f = (++ format) $ takeWhile (/= '.') f

checkArgsDecode :: FilePath -> Args
checkArgsDecode fp
  | checkFormat ".edx" fp   = Decode fp
  | otherwise               = Error "Invalid input file! You must use .edx!"

checkArgsEncode :: FilePath -> String -> Args
checkArgsEncode fp axis
  | cPLY && cAxis axis = Encode fp (string2Axis axis)
  | not cPLY           = Error "Invalid input file! You must use .ply!"
  | otherwise          = Error "Invalid axis! You must use X or Y or Z!"
  where cPLY = checkFormat ".ply" fp

checkFormat :: Format -> FilePath -> Bool
checkFormat fm fp = (== fm) $ dropWhile (/= '.') fp

string2Axis :: String -> Axis
string2Axis "X" = X
string2Axis "Y" = Y
string2Axis "Z" = Z

cAxis :: String -> Bool
cAxis "X" = True
cAxis "Y" = True
cAxis "Z" = True
cAxis _ = False

parseArgs :: [String] -> Args
parseArgs (operation:input:arg1:_)
  | operation == "-e" = checkArgsEncode input arg1
  | operation == "-d" = checkArgsDecode input
  | otherwise         = Error "Invalid operation!"
parseArgs _ = Error "Invalid arguments!"

--mainDebug :: IO ()
mainDebug = do
  let file = "test.edx"
  decodedContent <- B.readFile file
  (Right pc) <- pure $ buildPLY =<< decodeGeometry decodedContent
  B.writeFile "testeD.ply" pc
  return pc