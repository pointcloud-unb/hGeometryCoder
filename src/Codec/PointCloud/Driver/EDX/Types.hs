{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Codec.PointCloud.Driver.EDX.Types (
    EDX (..)
  , EDXHeader (..)
  , EDXPayload
  , ourMagicWord
  , currentVersion  ) where

import Codec.PointCloud.Utils 

import Flat
import Data.Word
import GHC.Generics (Generic)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

type EDXPayload = BL.ByteString
type Version = B.ByteString
ourMagicWord = "0x60D1AD1C"
currentVersion = "0.1.0.0"

data EDX = EDX { edxHeader :: EDXHeader
               , edxPayload :: EDXPayload }
           deriving (Eq, Show, Generic, Flat)

data EDXHeader = EDXHeader { edxMagicWord    :: B.ByteString
                           , edxAxis         :: Axis
                           , edxMode         :: Mode
                           , edxPCSize       :: PointCloudSizeBits
                           , edxCodecVersion :: Version
                           }
                 deriving (Eq, Show, Generic, Flat)

defaultEDXHeader :: EDXHeader
defaultEDXHeader = EDXHeader {
  edxMagicWord = ourMagicWord
  , edxAxis = X
  , edxMode = Normal
  , edxPCSize = 0
  , edxCodecVersion = currentVersion
  }

defaultEDX :: EDX
defaultEDX = EDX {
  edxHeader = defaultEDXHeader
  , edxPayload = BL.empty
  }

-- DEPRECATED
-- data Header = Header { axisH :: Axis
--                      , pcSizeH :: PointCloudSize}
--   deriving (Show)

-- data PCBitStream =
--   PCBitStream { padding :: Padding
--               , axis    :: Axis
--               , pcSizeS :: PointCloudSize
--               , payload :: B.ByteString}
--   deriving (Show)
