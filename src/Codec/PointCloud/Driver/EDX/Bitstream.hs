{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Codec.PointCloud.Driver.EDX.Bitstream (
    encodeEDX
  , decodeEDX
  , writeEDX
  , readEDX
  , buildEDX ) where

import Flat
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Codec.PointCloud.Driver.EDX.Types
import Codec.PointCloud.Utils
import Data.ByteString.Builder
import Data.Bits
import Data.Word

encodeEDX :: EDX -> B.ByteString
encodeEDX = flat

decodeEDX :: B.ByteString -> Either String EDX
decodeEDX bs =
  case unflat bs of
    Left (NotEnoughSpace _) -> Left "Bitstream is lacking something..."
    Left (TooMuchSpace   _) -> Left "Bitstream has too many bits..."
    Left (BadEncoding  _ _) -> Left "Bitstream encoding is wrong..."
    (Right edx) -> case edxMagicWord . edxHeader $ edx of
      ourMagicWord -> Right edx
      _ -> Left "Magic word is wrong..."

writeEDX :: FilePath -> EDX -> IO ()
writeEDX file edx = B.writeFile file $ encodeEDX edx

readEDX :: FilePath -> IO (Either String EDX)
readEDX file = decodeEDX <$> B.readFile file

buildEDX :: (Bin, PointCloudSize, Axis) -> Either String EDX
buildEDX (b, ps, a) = Right $ EDX (EDXHeader ourMagicWord a Normal bps currentVersion) dataBin
  where dataBin = toLazyByteString $ writeEDXData mempty b (Prelude.length b)
        bps = transcode $ integral2BitList (fromIntegral $ computeNBits ps :: Word8)

writeEDXData :: Builder -> Bin -> Int -> Builder
writeEDXData tcoded bin binSize
    | binSize >= 8   = writeEDXData (tcoded <> word8 (transcode bin)) (drop 8 bin) (binSize - 8)
    | binSize == 0   = tcoded
    | otherwise      = finishTranscode
        where padding = 8 - binSize
              finishTranscode = tcoded <> word8 (BL.foldl' (\ res b -> res `shiftL` 1 + b) 0 (BL.pack bin) `shiftL` padding)

transcode :: Bin -> Byte
transcode (b1:b2:b3:b4:b5:b6:b7:b8:_) =
    b1 `shiftL` 7 .|.
    b2 `shiftL` 6 .|.
    b3 `shiftL` 5 .|.
    b4 `shiftL` 4 .|.
    b5 `shiftL` 3 .|.
    b6 `shiftL` 2 .|.
    b7 `shiftL` 1 .|. b8

