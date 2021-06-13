
module CLI where

import Codec.PointCloud.Utils


type Format = String
data Args = Encode { inputPath :: FilePath
                    , axis :: Axis}
            | Decode { inputPath :: FilePath }
            | Parse { inputPath :: FilePath }
            | Error { errorMessage :: String }


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
parseArgs (operation:input:arg1)
  | operation == "-e" = checkArgsEncode input (head arg1)
  | operation == "-d" = checkArgsDecode input
  | operation == "-p" = Parse input
  | otherwise         = Error "Invalid operation!"
parseArgs _ = Error "Invalid arguments!"


