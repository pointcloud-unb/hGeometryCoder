{-# LANGUAGE OverloadedStrings #-}

module Codec.PointCloud.Compression.PLY
  ( pc2PLY
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

pc2PLY :: PointCloud -> PLY
pc2PLY pc = PLY header (writePLYData pc)
  where
    header =
      Header
        ASCII
        [ Element
            (B8.pack "vertex")
            (pcSize pc)
            [ ScalarProperty FloatT (B8.pack "x"),
              ScalarProperty FloatT (B8.pack "y"),
              ScalarProperty FloatT (B8.pack "z")
            ]
        ]

writePLYData :: PointCloud -> DataBlocks
writePLYData (PointCloud voxelList _) = fmap formatVoxel (toList voxelList)
  where
    formatVoxel voxel =
      let u = FloatS ((fromIntegral $ getU voxel) :: Float)
          v = FloatS ((fromIntegral $ getV voxel) :: Float)
          w = FloatS ((fromIntegral $ getW voxel) :: Float)
       in [u, v, w]

