{-# LANGUAGE OverloadedStrings #-}
module Data.PLY.Parser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (char)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Int (Int8, Int16)
import Data.Word (Word8, Word16, Word32)
import Data.Vector (Vector, fromList, replicateM)

import Data.PLY.Types


-- * ASCII parsers
-- | Parse a given element data.
-- elementData :: Element -> Parser (Vector (Vector Scalar))
-- elementData e = replicateM (elNum e)
--                   (skipComments *> (fromList <$> dataLine (elProps e)))
elementData :: Element -> Parser [Values]
elementData e = count (elNum e)
                  (skipComments *> dataLine (elProps e))

-- * Header parser
-- | Parse the PLY header
header :: Parser Header
header = Header <$> preamble <*> elements <* "end_header" <* endOfLine
  where preamble = "ply" *> endOfLine *> format <* skipSpace
        elements = many1 (skipComments *> element <* skipSpace)

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
dataLine :: [Property] -> Parser [Scalar]
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
