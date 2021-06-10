
module Codec.PointCloud.Types.PCBitStream where

import Codec.PointCloud.Utils
import qualified Data.ByteString as B

data Header = Header {axisH :: Axis
                    , pcSizeH :: PointCloudSize}
  deriving (Show)

data PCBitStream =
  PCBitStream { padding  :: Padding
            , axis   :: Axis
            , pcSizeS :: PointCloudSize
            , payload :: B.ByteString}
  deriving (Show)
