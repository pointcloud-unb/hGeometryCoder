{-# LANGUAGE OverloadedStrings #-}

module Codec.PointCloud.Driver.PLY.PLY
  ( parsePLY,
    formatPLY
  ) where


import qualified Data.ByteString as B (ByteString)
import Data.Attoparsec.ByteString.Char8
    ( parse, parseOnly, Result, IResult(Partial, Done, Fail) )
import qualified Data.ByteString.Char8 as B8 (pack)

import Codec.PointCloud.Driver.PLY.Parser ( elementData, header )
import Codec.PointCloud.Driver.PLY.Types
    (Property, Element(Element), Header(Header, hElems),
     PLY(PLY), DataBlocks, Format (ASCII), Property(ScalarProperty),
      ScalarType(FloatT), Scalar (FloatS))
import Control.Monad (forM, join)
import Codec.PointCloud.Types.PointCloud(PointCloud(PointCloud), pcSize)
import Codec.PointCloud.Types.Voxel (Voxel(getU, getV, getW))
import Data.Foldable (Foldable(toList))

formatPLY :: PointCloud -> PLY
formatPLY pc = PLY header (formatPLYData pc)
  where header = Header
                    ASCII
                    [Element (B8.pack "vertex") (pcSize pc)
                                 [ScalarProperty FloatT (B8.pack "x"),
                                  ScalarProperty FloatT (B8.pack "y"),
                                  ScalarProperty FloatT (B8.pack "z")]]

formatPLYData :: PointCloud -> DataBlocks
formatPLYData (PointCloud voxelList _) = fmap formatVoxel (toList voxelList)
  where formatVoxel voxel = [FloatS ((fromIntegral $ getU voxel)::Float),
                             FloatS ((fromIntegral $ getV voxel)::Float),
                             FloatS ((fromIntegral $ getW voxel)::Float)]



-- parsePLY :: B.ByteString -> Either String PLY
parsePLY b = do
          resultHeader <- loadHeader b
          case resultHeader of
            (Done r parsedHeader) ->
              let dataParser = forM (hElems parsedHeader) elementData
              in case parseOnly dataParser r of
                (Left _) -> Left "Invalid data"
                (Right x) -> return $ PLY parsedHeader (join x)
            _ -> Left "Something crazy happenned"

loadHeader :: B.ByteString -> Either String (Result Header)
loadHeader = result . parse header
  where
    result (Fail _ ctxt msg) = Left $ "Parse failed: " ++ msg ++ " in " ++ show ctxt
    result (Partial _) = Left "Incomplete header..."
    result doneHeader = Right doneHeader

getProperties :: Header -> [Property]
getProperties (Header _ []) = []
getProperties (Header _ ((Element _ _ p):_)) = p

getCounter :: Header -> Int
getCounter (Header _ []) = 0
getCounter (Header _ ((Element _ c _):_)) = c

