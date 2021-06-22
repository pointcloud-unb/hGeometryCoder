{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Codec.PointCloud.Driver.PLY.Parser
  ( parsePLY
  , parsePLY'
  , parsePLYV
  , parseVertexPLY
  , readPLY
  , unflatPLY
  , readFlatPLY
  , elementData
  , header
  ) where

import Codec.PointCloud.Driver.PLY.Types

import Flat
import Control.Applicative
import Control.Monad (join, forM, replicateM)
import Data.Char (ord)
import Data.Attoparsec.ByteString.Char8 hiding (char)
import qualified Data.ByteString.Char8 as B (ByteString, pack, readFile, drop) 
import Data.Int (Int8, Int16)
import Data.Word (Word8, Word16, Word32)
import Data.Foldable

import qualified Data.Sequence as S
import qualified Data.Vector as V

import System.IO.Unsafe (unsafePerformIO)
import Data.Either

parsePLY :: B.ByteString -> Either String PLY
parsePLY = parseOnly (ply <* endOfInput) 

-- parsePLYHeader1 :: B.ByteString -> Either String Header
-- parsePLYHeader1 = parseOnly header

-- PLY using Sequence for benchmarking --
parsePLY' :: B.ByteString -> Either String PLY'
parsePLY' = parseOnly (ply' <* endOfInput) 


-- PLY using Vector for benchmarking --
parsePLYV :: B.ByteString -> Either String PLYV
parsePLYV = parseOnly (plyV <* endOfInput) 

parseVertexPLY :: B.ByteString -> Either String PLY
parseVertexPLY = parseOnly (filteredPLY vertex <* endOfInput)
  where
    vertex = Element "vertex" 0
             [ (ScalarProperty CharT "x")
             , (ScalarProperty CharT "y")
             , (ScalarProperty CharT "z")
             ]

readPLY :: FilePath -> IO (Either String PLY)
readPLY file = parsePLY <$> B.readFile file

unflatPLY :: B.ByteString -> Decoded PLY
unflatPLY = unflat

readFlatPLY :: FilePath -> IO (Decoded PLY)
readFlatPLY file = unflatPLY <$> B.readFile file

-- Internals -- 

-- Top Level parsers --

-- * Header parser
-- | Parse the PLY header
header :: Parser Header
header = Header <$> preamble <*> elements <* "end_header" <* endOfLine
  where preamble = ("ply" <|> "PLY") *> endOfLine *> format <* skipSpace
        elements = many1 (skipComments *> element <* skipSpace)

ply :: Parser PLY
ply = do
  !parsedHeader <- header
  !dataBlocks <- join <$> forM (hElems parsedHeader) elementData
  return $! PLY parsedHeader dataBlocks

ply' :: Parser PLY'
ply' = do
  !parsedHeader <- header
  !dataBlocks <- join <$> forM (S.fromList $ hElems parsedHeader) elementData'
  return $! PLY' parsedHeader dataBlocks

plyV :: Parser PLYV
plyV = do
  !parsedHeader <- header
  !dataBlocks <- join <$> forM (V.fromList $ hElems parsedHeader) elementDataV
  return $! PLYV parsedHeader dataBlocks


filteredPLY :: Element -> Parser PLY
{-# INLINE filteredPLY #-}
filteredPLY searchElement = do
  !parsedHeader <- header
  let parsedEls = hElems parsedHeader
  dataBlocks <- join <$> (forM parsedEls $ takeDataBlockByElement searchElement)
  return $ PLY parsedHeader dataBlocks

takeDataBlockByElement :: Element -> Element -> Parser DataBlocks
{-# INLINE takeDataBlockByElement #-}
takeDataBlockByElement searchEl@(Element searchName _ searchProps) el@(Element name num props) =
  if name /= searchName
  then count num skipLine *> return []
  else count num (filteredDataLine searchProps props)
       --else elementData e

-- elementData :: Element -> Parser [DataLine]
-- {-# INLINE elementData #-}
-- elementData e = count (elNum e)
--                   (skipComments *> dataLine (elProps e))

s1 = Element "vertex" 0
     [ (ScalarProperty CharT "x")
     , (ScalarProperty CharT "y")
     , (ScalarProperty CharT "z")
     ]

s2 = Element "vertex" 0
     [ (ScalarProperty CharT "z")
     , (ScalarProperty CharT "y")
     , (ScalarProperty CharT "x")
     ]

s3 = Element "vertex" 0
     [ (ScalarProperty CharT "x")
     , (ScalarProperty CharT "red")
     , (ScalarProperty CharT "blue")
     ]

s4 = Element "face" 0
     [(ListProperty CharT CharT "vertex_index")]

s5 = Element "faces" 0
     [(ListProperty CharT CharT "vertex_index")]

s6 = Element "face" 0
     [(ListProperty CharT CharT "face_index")]


pHeader = fromRight undefined $ parseOnly header simple2
el1 = head . hElems $ pHeader 
el2 = head . drop 1 . hElems $ pHeader

simple2 :: B.ByteString
simple2 = "ply\nformat ascii 1.0\ncomment author: Greg Turk\ncomment object: another cube\nelement vertex 8\nproperty float x\nproperty float y\nproperty float z\nproperty uchar red\nproperty uchar green\nproperty uchar blue\nelement face 7\nproperty list uchar int vertex_index\nelement edge 5\nproperty int vertex1\nproperty int vertex2\nproperty uchar red\nproperty uchar green\nproperty uchar blue\nend_header\n0 0 0 255 0 0\n0 0 1 255 0 0\n0 1 1 255 0 0\n0 1 0 255 0 0\n1 0 0 0 0 255\n1 0 1 0 0 255\n1 1 1 0 0 255\n1 1 0 0 0 255\n3 0 1 2\n3 0 2 3\n4 7 6 5 4\n4 0 4 5 1\n4 1 5 6 2\n4 2 6 7 3\n4 3 7 4 0\n0 1 255 255 255\n1 2 255 255 255\n2 3 255 255 255\n3 0 255 255 255\n2 0 0 0 0\n"

simple2payload :: B.ByteString
simple2payload = "0 0 0 255 0 0\n0 0 1 255 0 0\n0 1 1 255 0 0\n0 1 0 255 0 0\n1 0 0 0 0 255\n1 0 1 0 0 255\n1 1 1 0 0 255\n1 1 0 0 0 255\n3 0 1 2\n3 0 2 3\n4 7 6 5 4\n4 0 4 5 1\n4 1 5 6 2\n4 2 6 7 3\n4 3 7 4 0\n0 1 255 255 255\n1 2 255 255 255\n2 3 255 255 255\n3 0 255 255 255\n2 0 0 0 0\n"

payload1 :: B.ByteString
payload1 = "0 0 0 255 0 0\n0 0 1 255 0 0\n0 1 1 255 0 0\n0 1 0 255 0 0\n1 0 0 0 0 255\n1 0 1 0 0 255\n1 1 1 0 0 255\n1 1 0 0 0 255\n"

payload2 :: B.ByteString
payload2 = "3 0 1 2\n3 0 2 3\n4 7 6 5 4\n4 0 4 5 1\n4 1 5 6 2\n4 2 6 7 3\n4 3 7 4 0\n"

payload3 :: B.ByteString
payload3 = "0 1 255 255 255\n1 2 255 255 255\n2 3 255 255 255\n3 0 255 255 255\n2 0 0 0 0\n"





filteredDataLine :: [Property] -> [Property] -> Parser DataLine
{-# INLINE filteredDataLine #-}
filteredDataLine searchProps props = concat <$> traverse (propertyDataByName searchNames) props
  where searchNames = getPropertyName <$> searchProps
        getPropertyName prop
          | (ScalarProperty _ name) <- prop = name
          | (ListProperty _ _ name) <- prop = name

propertyDataByName :: [B.ByteString] -> Property -> Parser [Scalar]
{-# INLINE propertyDataByName #-}
propertyDataByName searchNames (ScalarProperty propType propName) =
  if propName `elem` searchNames
  then do
    !x <- scalar propType <* skipSpace
    return [x]
  else do
    skipScalar
    return []
propertyDataByName searchNames (ListProperty indexType propType propName) = do 
  !x <- scalar indexType <* skipSpace
  let !c = scalarInt x
  if propName `elem` searchNames
    then
    replicateM c (scalar propType <* skipSpace)
    else do
    replicateM c (skipScalar <* skipSpace)
    return []
    




-- Low level parsers -- 

elementData :: Element -> Parser [DataLine]
{-# INLINE elementData #-}
elementData e = count (elNum e)
                  (skipComments *> dataLine (elProps e))


elementData' :: Element -> Parser DataBlocks'
{-# INLINE elementData' #-}
elementData' e = S.replicateM (elNum e) (skipComments *> dataLine' (elProps e))

elementDataV :: Element -> Parser DataBlocksV
{-# INLINE elementDataV #-}
elementDataV e = V.replicateM (elNum e) (skipComments *> dataLineV (elProps e))



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



dataLine :: [Property] -> Parser DataLine
{-# INLINE dataLine #-}
dataLine ps = concat <$> traverse propertyData ps


propertyData :: Property -> Parser [Scalar]
{-# INLINE propertyData #-}
propertyData (ScalarProperty propType _) = do
  !x <- scalar propType <* skipSpace
  return [x]
propertyData (ListProperty indexType propType _) = do
  !x <- scalar indexType <* skipSpace
  let !c = scalarInt x
  replicateM c (scalar propType <* skipSpace)


dataLine' :: [Property] -> Parser DataLine'
{-# INLINE dataLine' #-}
dataLine' ps = fold <$> traverse propertyData' ps


propertyData' :: Property -> Parser (S.Seq Scalar)
{-# INLINE propertyData' #-}
propertyData' (ScalarProperty propType _) = do
  !x <- scalar propType <* skipSpace
  return $ S.singleton x
propertyData' (ListProperty indexType propType _) = do
  !x <- scalar indexType <* skipSpace
  let !c = scalarInt x
  S.replicateM c (scalar propType <* skipSpace)



dataLineV :: [Property] -> Parser DataLineV
{-# INLINE dataLineV #-}
dataLineV ps = V.concat <$> traverse propertyDataV ps


propertyDataV :: Property -> Parser (V.Vector Scalar)
{-# INLINE propertyDataV #-}
propertyDataV (ScalarProperty propType _) = do
  !x <- scalar propType <* skipSpace
  return $ V.singleton x
propertyDataV (ListProperty indexType propType _) = do
  !x <- scalar indexType <* skipSpace
  let !c = scalarInt x
  V.replicateM c (scalar propType <* skipSpace)
  


-- | Extract an Int from the Scalar types. Return 0 if float or double.
scalarInt :: Scalar -> Int
{-# INLINE scalarInt #-}
scalarInt !(CharS n)   = fromIntegral n
scalarInt !(UcharS n)  = fromIntegral n
scalarInt !(ShortS n)  = fromIntegral n
scalarInt !(UshortS n) = fromIntegral n
scalarInt !(IntS n)    = n
scalarInt !(UintS n)   = fromIntegral n
scalarInt _ = 0

-- * Scalar parser
scalar :: ScalarType -> Parser Scalar
{-# INLINE scalar #-}
scalar !CharT   = CharS   <$> char
scalar !UcharT  = UcharS  <$> uchar
scalar !ShortT  = ShortS  <$> int16
scalar !UshortT = UshortS <$> uint16
scalar !IntT    = IntS    <$> int
scalar !UintT   = UintS   <$> uint
scalar !FloatT  = FloatS  <$> float
scalar !DoubleT = DoubleS <$> double

-- * Numeric parsers
char :: Parser Int8
{-# INLINE char #-}
char = signed decimal

uchar :: Parser Word8
{-# INLINE uchar #-}
uchar = decimal

int16 :: Parser Int16
{-# INLINE int16 #-}
int16 = signed decimal

uint16 :: Parser Word16
{-# INLINE uint16 #-}
uint16 = decimal

int :: Parser Int
{-# INLINE int #-}
int = signed decimal

uint :: Parser Word32
{-# INLINE uint #-}
uint = decimal

float :: Parser Float
{-# INLINE float #-}
float = realToFrac <$> double

-- double implemented by attoparsec already

-- * Utility parsers
skipComments :: Parser ()
skipComments = skipSpace *> ("comment" *> takeLine *> skipComments) <|> pure ()

skipLine :: Parser ()
skipLine = (skipWhile $ not . isEndOfLine . c2w) <* skipSpace

skipScalar :: Parser ()
skipScalar = (skipWhile $ not . isSpace) <* skipSpace

skipElementData :: Element -> Parser ()
skipElementData e = count (elNum e) (skipComments *> takeLine) *> pure ()

takeLine :: Parser B.ByteString
takeLine = takeTill $ isEndOfLine . c2w

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

