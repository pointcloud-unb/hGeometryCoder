module Data.Structures.PCBitStream where

import Data.Utils
import qualified Data.ByteString as B

data PCBitStream =
  PCBitStream { padding  :: Padding
            , axis   :: Axis
            , pcSize :: PointCloudSize
            , payload :: B.ByteString}
  deriving (Show)
