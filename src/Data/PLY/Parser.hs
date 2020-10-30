{-# LANGUAGE OverloadedStrings #-}
module Data.PLY.Parser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString, pack)
import Data.PLY.Types


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
element = Element <$> ("element " *> takeTill isSpace)
                  <*> (skipSpace *> int <* skipSpace)
                  <*> (many' property)

property :: Parser Property
property = skipComments *> (scalarProperty <|> listProperty)
  where scalarProperty = ScalarProperty <$> ("property " *> scalarType) <*> takeLine
        listProperty = ListProperty <$>
                       ("property list " *> scalarType) <*>
                       (skipSpace *> scalarType <* skipSpace) <*>
                       takeLine

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


-- * Numeric type parsers
--char
--uchar
int :: Parser Int
int = signed decimal

--uint
--int16
--uint16
--float
--double


-- * Utility parsers
skipComments :: Parser ()
skipComments = skipSpace *> ("comment " *> takeLine *> skipComments) <|> pure ()

takeLine :: Parser ByteString
takeLine = pack <$> manyTill anyChar endOfLine
