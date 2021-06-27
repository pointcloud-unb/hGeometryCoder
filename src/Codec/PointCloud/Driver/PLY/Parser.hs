{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Codec.PointCloud.Driver.PLY.Parser where

import Codec.PointCloud.Driver.PLY.Types
import Codec.PointCloud.Types.Voxel
import qualified Codec.PointCloud.Types.PointCloud as PC 

import Flat
import Control.Applicative
import Control.Monad (join, forM, replicateM, (<$!>), foldM)
import Data.Char (ord)
import Data.Attoparsec.ByteString.Char8 hiding (char, take)
import Data.Int (Int8, Int16)
import Data.Word (Word8, Word16, Word32)
import Data.Either
import Data.Maybe (maybe, catMaybes)

import qualified Data.ByteString.Char8 as B


parsePLY :: B.ByteString -> Either String PLY
parsePLY = parseOnly (ply <* endOfInput) 


parseVertexPLY :: B.ByteString -> Either String PLY
parseVertexPLY = parseOnly (filteredPLY vertex <* endOfInput)
  where
    vertex = Element "vertex" 0
             [ (ScalarProperty CharT "x")
             , (ScalarProperty CharT "y")
             , (ScalarProperty CharT "z")
             ]

parsePointCloud :: B.ByteString -> Either String PC.PointCloud
parsePointCloud = parseOnly (filteredPLY' vertex <* endOfInput)
  where
    vertex = Element "vertex" 0
             [ (ScalarProperty CharT "x")
             , (ScalarProperty CharT "y")
             , (ScalarProperty CharT "z")
             ]

--parsePointCloud' :: B.ByteString -> Either String PC.PointCloud
parsePointCloud' = parseOnly (filteredPLY'' vertex <* endOfInput)
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
  where preamble = ("ply" *> endOfLine) *> format <* skipSpace
        elements = many1 (skipComments *> element <* skipSpace)

ply :: Parser PLY
ply = do
  !parsedHeader <- header
  !dataBlocks <- join <$> forM (hElems parsedHeader) elementData
  return $! PLY parsedHeader dataBlocks


filteredPLY :: Element -> Parser PLY
{-# INLINE filteredPLY #-}
filteredPLY searchElement = do
  !parsedHeader <- header
  !dataBlocks <- join <$> (forM (hElems parsedHeader) $ takeDataBlockByElement searchElement)
  return $ PLY parsedHeader dataBlocks

filteredPLY' :: Element -> Parser PC.PointCloud
{-# INLINE filteredPLY' #-}
filteredPLY' searchElement = do
  !parsedHeader <- header
  PC.fromList . catMaybes . join <$> forM (hElems parsedHeader) (takeDataBlockByElement' searchElement)
  
--filteredPLY'' :: Element -> Parser PC.PointCloud
{-# INLINE filteredPLY'' #-}
filteredPLY'' searchElement = do
  !parsedHeader <- header
  -- PC.fromList' . join <$> forM (hElems parsedHeader) (takeDataBlockByElement' searchElement) 
  forM (hElems parsedHeader) (takeDataBlockByElement'' searchElement)




-- Low level parsers -- 

takeDataBlockByElement :: Element -> Element -> Parser DataBlocks
{-# INLINE takeDataBlockByElement #-}
takeDataBlockByElement (Element searchName _ searchProps) (Element name num props) =
  if name /= searchName
  then count num skipLine *> return []
  else let !ps = fromRight undefined $ foldSelect (propName <$> searchProps) (Left <$> props)
       in
         if allScalars searchProps props
         then count num (filteredDataLineScalars ps)
         else count num (filteredDataLine ps)

takeDataBlockByElement' :: Element -> Element -> Parser [Maybe Voxel]
{-# INLINE takeDataBlockByElement' #-}
takeDataBlockByElement' (Element searchName _ searchProps) (Element name num props) =
  if name /= searchName
  then count num skipLine *> return []
  else let !ps = fromRight undefined $ foldSelect (propName <$> searchProps) (Left <$> props)
       in
         if allScalars searchProps props
         then count num (filteredDataLineScalars' ps)
         else count num (filteredDataLine' ps)


--takeDataBlockByElement'' :: Element -> Element -> Parser ([Voxel], Int)
takeDataBlockByElement'' :: Element -> Element -> Parser PC.PointCloud
{-# INLINE takeDataBlockByElement'' #-}
takeDataBlockByElement'' (Element searchName _ searchProps) (Element name num props) =
  if name /= searchName
  then count num skipLine *> mempty
  else let !ps = fromRight undefined $ foldSelect (propName <$> searchProps) (Left <$> props)
           accParser num parser = foldM worker ([], 0) (replicate num parser) 
           worker (xs, size) parser = do
             !x <- parser
             maybe (return (xs, size)) (\(voxel, vxSize) -> return (voxel:xs, max size vxSize)) x
       in
         if allScalars searchProps props
         then do
           parsed <- accParser num (filteredDataLine'' ps)
           return $ PC.fromList' parsed
         else do
           parsed <- accParser num (filteredDataLine'' ps)
           return $ PC.fromList' parsed

           
filteredDataLine :: [Either Property Property] -> Parser DataLine
{-# INLINE filteredDataLine #-}
filteredDataLine ps = concat <$> traverse propertyDataByName ps

filteredDataLine' :: [Either Property Property] -> Parser (Maybe Voxel)
{-# INLINE filteredDataLine' #-}
filteredDataLine' ps = mkVoxel . concat <$> traverse propertyDataByName ps
  where
    mkVoxel (s1:s2:s3:_) = Just $ Voxel (scalarInt s1) (scalarInt s2) (scalarInt s3)
    mkVoxel _ = Nothing

filteredDataLine'' :: [Either Property Property] -> Parser (Maybe (Voxel, Int))
{-# INLINE filteredDataLine'' #-}
filteredDataLine'' ps = mkVoxel . concat <$> traverse propertyDataByName ps
  where
    mkVoxel (s1:s2:s3:_) = Just (Voxel (scalarInt s1) (scalarInt s2) (scalarInt s3), maximum $ scalarInt <$> [s1,s2,s3] )
    mkVoxel _ = Nothing
                      
                
propertyDataByName :: Either Property Property -> Parser [Scalar]
{-# INLINE propertyDataByName #-}
propertyDataByName (Left (ScalarProperty _ _)) =
  skipScalar *> return []
propertyDataByName (Right (ScalarProperty propType _)) =
  do
    !x <- scalar propType <* skipSpace
    return [x]
propertyDataByName (Left (ListProperty indexType _ _)) =
  do
    !x <- scalar indexType <* skipSpace
    let !c = scalarInt x
    count c (skipScalar <* skipSpace) *> return []
propertyDataByName (Right (ListProperty indexType propType _)) =
  do
    !x <- scalar indexType <* skipSpace
    let !c = scalarInt x
    count c (scalar propType <* skipSpace)

filteredDataLineScalars :: [Either Property Property] -> Parser DataLine
{-# INLINE filteredDataLineScalars #-}
filteredDataLineScalars ps = catMaybes <$> traverse propertyDataByNameScalars ps

filteredDataLineScalars' :: [Either Property Property] -> Parser (Maybe Voxel)
{-# INLINE filteredDataLineScalars' #-}
filteredDataLineScalars' ps = mkVoxel . catMaybes <$> traverse propertyDataByNameScalars ps
  where
    mkVoxel (s1:s2:s3:_) = Just $ Voxel (scalarInt s1) (scalarInt s2) (scalarInt s3)
    mkVoxel _ = Nothing


propertyDataByNameScalars :: Either Property Property -> Parser (Maybe Scalar)
{-# INLINE propertyDataByNameScalars #-}
propertyDataByNameScalars (Left (ScalarProperty _ _)) =
  skipScalar *> return Nothing
propertyDataByNameScalars (Right (ScalarProperty propType _)) =
  do
    !x <- scalar propType <* skipSpace
    return $ Just x


elementData :: Element -> Parser [DataLine]
{-# INLINE elementData #-}
elementData e = count (elNum e) (dataLine (elProps e))


format :: Parser Format
format = "format" *> skipSpace *> (ascii <|> binaryLE <|> binaryBE)
  where ascii    = ASCII    <$ ("ascii" *> skipSpace *> "1.0")
        binaryLE = BinaryLE <$ ("binary_little_endian" *> skipSpace *> "1.0")
        binaryBE = BinaryBE <$ ("binary_big_endian" *> skipSpace *> "1.0")

element :: Parser Element
element = Element <$> (skipSpace *> "element" *> skipSpace *> takeTill isSpace)
                  <*> (skipSpace *> decimal <* skipSpace)
                  <*> (many1 property)

property :: Parser Property
property = skipComments *> (scalarProperty <|> listProperty)
  where
    property' = skipSpace *> "property" <* skipSpace
    propertyL' = skipSpace *> "property list" <* skipSpace
    scalarType' = scalarType <* skipSpace
    name' = takeToken <* skipSpace
    scalarProperty = ScalarProperty <$> (property' *> scalarType') <*> name'
    listProperty = ListProperty <$> (propertyL' *> scalarType') <*> scalarType' <*> name'
    

-- * Scalar types parser
scalarType :: Parser ScalarType
scalarType = choice $               -- Sensitive to the order of the list... 
             [ FloatT  <$ "float32" -- Try floats first, seems to be prefered 
             , DoubleT <$ "float64" -- real world PLY files... 
             , DoubleT <$ "double"
             , FloatT  <$ "float"   -- catch all: should come after other floats.
             , CharT   <$ "int8"
             , ShortT  <$ "int16"
             , IntT    <$ "int32"
             , IntT    <$ "int"     -- should come after other ints.
             , CharT   <$ "char"
             , ShortT  <$ "short"
             , UcharT  <$ "uchar"
             , UshortT <$ "ushort"
             , UcharT  <$ "uint8"
             , UshortT <$ "uint16"
             , UintT   <$ "uint32"
             , UintT   <$ "uint"    -- should come after other uints. 
             ]


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


-- * Scalar parser
scalar :: ScalarType -> Parser Scalar
{-# INLINE scalar #-}
scalar CharT   = CharS   <$!> signed decimal
scalar UcharT  = UcharS  <$!> decimal
scalar ShortT  = ShortS  <$!> signed decimal
scalar UshortT = UshortS <$!> decimal
scalar IntT    = IntS    <$!> signed decimal
scalar UintT   = UintS   <$!> decimal
scalar FloatT  = FloatS  <$!> (realToFrac <$> double)
scalar DoubleT = DoubleS <$!> double


-- * Utility parsers and functions
skipComments :: Parser ()
{-# INLINE skipComments #-}
skipComments = skipSpace *> ("comment" *> takeLine *> skipComments) <|> pure ()

skipLine :: Parser ()
{-# INLINE skipLine #-}
skipLine = (skipWhile $ not . isEndOfLine . c2w) <* skipSpace

skipScalar :: Parser ()
{-# INLINE skipScalar #-}
skipScalar = (skipWhile $ not . isSpace) <* skipSpace

skipElementData :: Element -> Parser ()
skipElementData e = count (elNum e) (skipComments *> takeLine) *> pure ()

skipRestOfLine :: Parser ()
skipRestOfLine = skipWhile (not . isEndOfLine . c2w) >> endOfLine

takeLine :: Parser B.ByteString
{-# INLINE takeLine #-}
takeLine = takeTill $ isEndOfLine . c2w

takeToken :: Parser B.ByteString
{-# INLINE takeToken #-}
takeToken = Data.Attoparsec.ByteString.Char8.takeWhile (not . isSpace)

c2w :: Char -> Word8
{-# INLINE c2w #-}
c2w = fromIntegral . ord

-- | Extract an Int from the Scalar types. Rounds if float or double.
scalarInt :: Scalar -> Int
{-# INLINE scalarInt #-}
scalarInt (CharS n)   = fromIntegral n
scalarInt (UcharS n)  = fromIntegral n
scalarInt (ShortS n)  = fromIntegral n
scalarInt (UshortS n) = fromIntegral n
scalarInt (IntS n)    = n
scalarInt (UintS n)   = fromIntegral n
scalarInt (FloatS n)  = round n
scalarInt (DoubleS n) = round n


foldSelect :: [B.ByteString] -> [Either Property Property] -> Either String [Either Property Property]
foldSelect [] ps = Right ps
foldSelect (n:ns) ps = do
  ps' <- select n ps
  foldSelect ns ps'
 
select :: B.ByteString -> [Either Property Property] -> Either String [Either Property Property]
select name ps = if switched
                 then Right $ p'
                 else Left $ "Property " <> B.unpack name <> " not found..."
  where
    f p = if (propName' p == name)
          then (switchEither p, True)
          else (p, False)
    (p', bs) = unzip . fmap f $  ps
    switched = or bs

propName' :: Either Property Property -> B.ByteString
propName' = either propName propName

propName :: Property -> B.ByteString
propName (ScalarProperty _ name) = name
propName (ListProperty _ _ name) = name
 
switchEither :: Either a a -> Either a a
switchEither = either Right Right

isScalarProperty :: Property -> Bool
{-# INLINE isScalarProperty #-}
isScalarProperty (ScalarProperty _ _) = True
isScalarProperty _ = False

allScalars :: [Property] -> [Property] -> Bool
{-# INLINE allScalars #-}
allScalars searchProps props = p && s
  where
    s = and $ isScalarProperty <$> searchProps
    p = and $ isScalarProperty <$> props
