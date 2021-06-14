{-# LANGUAGE OverloadedStrings #-}

module Codec.PointCloud.Driver.PLY.Output (putPLY, writePLY) where

import Codec.PointCloud.Driver.PLY.Types
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as B (ByteString, writeFile)


-- ply1 = PLY {plyHeader = Header {hFormat = ASCII, hElems = [Element {elName = "vertex", elNum = 12, elProps = [ScalarProperty {sPropType = FloatT, sPropName = "x"},ScalarProperty {sPropType = FloatT, sPropName = "y"},ScalarProperty {sPropType = FloatT, sPropName = "z"}]}]}, plyData = [[FloatS 1.0,FloatS 0.0,FloatS 2.0],[FloatS 1.0,FloatS 0.0,FloatS 3.0],[FloatS 1.0,FloatS 1.0,FloatS 2.0],[FloatS 1.0,FloatS 1.0,FloatS 3.0],[FloatS 2.0,FloatS 1.0,FloatS 1.0],[FloatS 2.0,FloatS 1.0,FloatS 2.0],[FloatS 2.0,FloatS 2.0,FloatS 1.0],[FloatS 2.0,FloatS 2.0,FloatS 2.0],[FloatS 3.0,FloatS 2.0,FloatS 0.0],[FloatS 3.0,FloatS 2.0,FloatS 1.0],[FloatS 3.0,FloatS 3.0,FloatS 0.0],[FloatS 3.0,FloatS 3.0,FloatS 1.0]]}

-- header1 = plyHeader ply1


-- ply2 = PLY {plyHeader = Header {hFormat = ASCII, hElems = [Element {elName = "vertex", elNum = 12, elProps = [ScalarProperty {sPropType = FloatT, sPropName = "x"},ScalarProperty {sPropType = FloatT, sPropName = "y"},ScalarProperty {sPropType = FloatT, sPropName = "z"}]}, Element {elName = "faces", elNum = 12, elProps = [ScalarProperty {sPropType = FloatT, sPropName = "x"},ScalarProperty {sPropType = FloatT, sPropName = "y"},ScalarProperty {sPropType = FloatT, sPropName = "z"}]}, Element {elName = "triangles", elNum = 12, elProps = [ScalarProperty {sPropType = FloatT, sPropName = "x"},ScalarProperty {sPropType = FloatT, sPropName = "y"},ScalarProperty {sPropType = FloatT, sPropName = "z"}]}]}, plyData = [[FloatS 1.0,FloatS 0.0,FloatS 2.0],[FloatS 1.0,FloatS 0.0,FloatS 3.0],[FloatS 1.0,FloatS 1.0,FloatS 2.0],[FloatS 1.0,FloatS 1.0,FloatS 3.0],[FloatS 2.0,FloatS 1.0,FloatS 1.0],[FloatS 2.0,FloatS 1.0,FloatS 2.0],[FloatS 2.0,FloatS 2.0,FloatS 1.0],[FloatS 2.0,FloatS 2.0,FloatS 2.0],[FloatS 3.0,FloatS 2.0,FloatS 0.0],[FloatS 3.0,FloatS 2.0,FloatS 1.0],[FloatS 3.0,FloatS 3.0,FloatS 0.0],[FloatS 3.0,FloatS 3.0,FloatS 1.0]]}

-- header2 = plyHeader ply2


putPLY :: PLY -> B.ByteString
putPLY = toLazyByteString . ply

writePLY :: FilePath -> PLY -> IO ()
writePLY file = B.writeFile file . putPLY


ply :: PLY -> Builder
ply (PLY plyHeader plyData) =
  header plyHeader <> newline <>
  datablocks plyData

header :: Header -> Builder
header (Header hFormat hElems) =
  "ply" <> newline <>
  format hFormat <> newline <>
  mconcat [element e | e <- hElems] <>
  "end_header"

datablocks :: DataBlocks -> Builder
datablocks [] = mempty
datablocks (d:ds) =
  dataline d <> mconcat [newline <> dataline l | l <- ds]

dataline :: DataLine -> Builder
dataline [] = mempty
dataline (s:ss) =
  scalar s <> mconcat [sep <> scalar s' | s' <- ss]

format :: Format -> Builder
format ASCII = "format ascii 1.0"
format BinaryLE = "format binary_little_endian 1.0"
format BinaryBE = "format binary_big_endian 1.0"

element :: Element -> Builder
element (Element elName elNum elProps) =
  "element" <> sep <> byteString elName <> sep <> intDec elNum <> newline <>
  mconcat [property p <> newline | p <- elProps]

property :: Property -> Builder
property (ScalarProperty propType propName) =
  "property" <> sep <>
  scalarType propType <> sep <>
  byteString propName
property (ListProperty propIndexType propType propName) =
  "property list" <> sep <>
  scalarType propIndexType <> sep <>
  scalarType propType <> sep <>
  byteString propName

scalarType :: ScalarType -> Builder
scalarType CharT   = "char"
scalarType UcharT  = "uchar"
scalarType ShortT  = "short"
scalarType UshortT = "ushort"
scalarType IntT    = "int"
scalarType UintT   = "uint"
scalarType FloatT  = "float"
scalarType DoubleT = "double"

scalar :: Scalar -> Builder
scalar (CharS n)   = int8Dec n
scalar (UcharS n)  = word8Dec n
scalar (ShortS n)  = int16Dec n
scalar (UshortS n) = word16Dec n
scalar (IntS n)    = intDec n
scalar (UintS n)   = word32Dec n
scalar (FloatS n)  = floatDec n
scalar (DoubleS n) = doubleDec n

-- Utils --
sep :: Builder
{-# INLINE sep #-}
sep = " "

newline :: Builder
newline = charUtf8 '\n'

--sepBy :: Builder -> [Builder] -> Builder
