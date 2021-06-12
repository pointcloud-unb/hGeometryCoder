{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Codec.PointCloud.Types.PCBitStream (
    PCBitStream (..)
  , Header(..) ) where

import Codec.PointCloud.Utils 

import Flat
import Data.Word
import GHC.Generics (Generic)

import qualified Data.ByteString as B



type EDXPayload = B.ByteString
type EDXPadding = Word8
type EDXPointCloudSize = Int

data EDXAxis = X1 | Y1 | Z1
  deriving (Show, Eq, Generic, Flat)


data EDXError
  = EDXBadEncoding String
  | EDXNotEnoughData String
  | EDXExcessData String
  deriving (Show)



data EDX = EDX { edxHeader :: EDXHeader
               , edxPayload :: EDXPayload }
           deriving (Eq, Show, Generic, Flat)

data EDXHeader = EDXHeader { edxMagicWord :: B.ByteString
                           , edxAxis    :: EDXAxis
                           , edxPCSize  :: EDXPointCloudSize
                           }
                 deriving (Eq, Show, Generic, Flat)

defaultEDXHeader :: EDXHeader
defaultEDXHeader = EDXHeader {
  edxMagicWord = "EDX"
  , edxAxis = X1
  , edxPCSize = 0
  }

defaultEDX :: EDX
defaultEDX = EDX {
  edxHeader = defaultEDXHeader
  , edxPayload = B.empty
  }

encodeEDX :: EDX -> B.ByteString
encodeEDX edx = flat edx

decodeEDX :: B.ByteString -> Either String EDX
decodeEDX bs =
  case unflat bs of
    Left (NotEnoughSpace _) -> Left "Bitstream is lacking something..."
    Left (TooMuchSpace   _) -> Left "Bitstream has too many bits..."
    Left (BadEncoding  _ _) -> Left "Bitstream encoding is wrong..."
    (Right edx) -> case edxMagicWord . edxHeader $ edx of
      "EDX" -> Right edx
      otherwise -> Left "Magic word is wrong..."


writeEDX :: FilePath -> EDX -> IO ()
writeEDX file edx = B.writeFile file $ encodeEDX edx

readEDX :: FilePath -> IO (Either String EDX)
readEDX file = decodeEDX <$> B.readFile file


data Header = Header { axisH :: Axis
                     , pcSizeH :: PointCloudSize}
  deriving (Show)

data PCBitStream =
  PCBitStream { padding :: Padding
              , axis    :: Axis
              , pcSizeS :: PointCloudSize
              , payload :: B.ByteString}
  deriving (Show)
