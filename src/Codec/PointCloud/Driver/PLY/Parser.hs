{-# LANGUAGE OverloadedStrings #-}

module Codec.PointCloud.Driver.PLY.Parser where

import Codec.PointCloud.Driver.PLY.Types

import Control.Applicative
import Control.Monad (join, forM)
import Data.Attoparsec.ByteString.Char8 hiding (char)
import Data.ByteString.Char8 (ByteString, pack, readFile, drop)
import Data.Int (Int8, Int16)
import Data.Word (Word8, Word16, Word32)
import Data.Vector (Vector, fromList, replicateM)

import qualified Data.Sequence as S ( (|>), empty, fromList, replicateM)

import Data.Either

import System.IO.Unsafe

e = [Element {elName = "faces", elNum = 5, elProps = [ScalarProperty {sPropType = FloatT, sPropName = "x2"},ScalarProperty {sPropType = FloatT, sPropName = "y2"}]},Element {elName = "vertexCoisa", elNum = 10, elProps = [ScalarProperty {sPropType = FloatT, sPropName = "x"},ScalarProperty {sPropType = FloatT, sPropName = "y"},ScalarProperty {sPropType = FloatT, sPropName = "z"},ScalarProperty {sPropType = UcharT, sPropName = "red"},ScalarProperty {sPropType = UcharT, sPropName = "green"},ScalarProperty {sPropType = UcharT, sPropName = "blue"}]}]

vertex = "vertex" :: ByteString

file = unsafePerformIO $ Data.ByteString.Char8.readFile  "assets/simple.ply"
fileData = Data.ByteString.Char8.drop 101 file

pHeader = fromRight undefined $ parseOnly header file
e1 = head $ hElems pHeader
e1props = elProps e1

-- * Header parser
-- | Parse the PLY header
header :: Parser Header
header = Header <$> preamble <*> elements <* "end_header" <* endOfLine
  where preamble = "ply" *> endOfLine *> format <* skipSpace
        elements = many1 (skipComments *> element <* skipSpace)

ply :: Parser PLY
ply = do
  parsedHeader <- header
  let dataParser = join <$> forM (hElems parsedHeader) elementData
  dataBlocks <- dataParser
  return $ PLY parsedHeader dataBlocks

ply' :: Parser PLY'
ply' = do
  parsedHeader <- header
  let dataParser = join <$> forM (S.fromList $ hElems parsedHeader) elementData'
  dataBlocks <- dataParser
  return $ PLY' parsedHeader dataBlocks



-- * ASCII parsers
-- | Parse a given element data.
-- elementData :: Element -> Parser (Vector (Vector Scalar))
-- elementData e = replicateM (elNum e)
--                   (skipComments *> (fromList <$> dataLine (elProps e)))
elementData :: Element -> Parser [DataLine]
elementData e = count (elNum e)
                  (skipComments *> dataLine (elProps e))

elementData' :: Element -> Parser DataBlocks'
elementData' e = S.replicateM (elNum e) (skipComments *> dataLine' (elProps e))


format :: Parser Format
format = "format" *> skipSpace *> (ascii <|> binaryLE <|> binaryBE)
  where ascii    = ASCII    <$ "ascii 1.0"
        binaryLE = BinaryLE <$ "binary_little_endian 1.0"
        binaryBE = BinaryBE <$ "binary_big_endian 1.0"

element :: Parser Element
element = Element <$> (skipSpace *> "element " *> takeTill isSpace)
                  <*> (skipSpace *> int <* skipSpace)
                  <*> (many' property)

property :: Parser Property
property = skipComments *> (scalarProperty <|> listProperty)
  where scalarProperty = ScalarProperty <$> (skipSpace *> "property " *> scalarType) <*> takeLine
        listProperty = ListProperty <$>
                       ("property list " *> scalarType) <*>
                       (skipSpace *> scalarType <* skipSpace) <*>
                       takeLine

-- * Scalar types parser
scalarType :: Parser ScalarType
scalarType = choice $
             [ CharT   <$ ("char "   <|> "int8 ")
             , UcharT  <$ ("uchar "  <|> "uint8 ")
             , ShortT  <$ ("short "  <|> "int16 ")
             , UshortT <$ ("ushort " <|> "uint16 ")
             , IntT    <$ ("int "    <|> "int32 ")
             , UintT   <$ ("uint "   <|> "uint32 ")
             , FloatT  <$ ("float "  <|> "float32 ")
             , DoubleT <$ ("double " <|> "float64 ") ]

-- * Data parser
dataLine :: [Property] -> Parser DataLine
dataLine = getData []
  where
    getData ds [] = pure (reverse ds)
    getData ds (ScalarProperty t _:ps) = do x <- scalar t
                                            skipSpace
                                            getData (x:ds) ps
    getData _ (ListProperty th td _:_) = do x <- scalar th
                                            skipSpace
                                            let c = scalarInt x
                                            count c (scalar td <* skipSpace)


-- * Data parser
dataLine' :: [Property] -> Parser DataLine'
dataLine' ps = getDataLine S.empty ps 
  where
    getDataLine dl [] = return dl
    getDataLine dl (ScalarProperty propT _:ps) = do
      x <- scalar propT
      skipSpace
      getDataLine (dl S.|> x) ps
    -- The following considers that we don't get scalar properties after list properties.
    getDataLine dl (ListProperty indexT propT _:_) = do
      x <- scalar indexT
      skipSpace
      let c = scalarInt x
      S.fromList <$> count c (scalar propT <* skipSpace)


-- | Extract an Int from the Scalar types. Return 0 if float or double.
scalarInt :: Scalar -> Int
scalarInt (CharS n)   = fromIntegral n
scalarInt (UcharS n)  = fromIntegral n
scalarInt (ShortS n)  = fromIntegral n
scalarInt (UshortS n) = fromIntegral n
scalarInt (IntS n)    = n
scalarInt (UintS n)   = fromIntegral n
scalarInt _ = 0

-- * Scalar parser
scalar :: ScalarType -> Parser Scalar
scalar CharT   = CharS   <$> char
scalar UcharT  = UcharS  <$> uchar
scalar ShortT  = ShortS  <$> int16
scalar UshortT = UshortS <$> uint16
scalar IntT    = IntS    <$> int
scalar UintT   = UintS   <$> uint
scalar FloatT  = FloatS  <$> float
scalar DoubleT = DoubleS <$> double

-- * Numeric parsers
char :: Parser Int8
char = signed decimal

uchar :: Parser Word8
uchar = decimal

int16 :: Parser Int16
int16 = signed decimal

uint16 :: Parser Word16
uint16 = decimal

int :: Parser Int
int = signed decimal

uint :: Parser Word32
uint = decimal

float :: Parser Float
float = realToFrac <$> double

-- double implemented by attoparsec already

-- * Utility parsers
skipComments :: Parser ()
skipComments = skipSpace *> ("comment " *> takeLine *> skipComments) <|> pure ()

skipElementData :: Element -> Parser ()
skipElementData e = count (elNum e) (skipComments *> takeLine) *> pure ()

takeLine :: Parser ByteString
takeLine = pack <$> manyTill anyChar endOfLine
