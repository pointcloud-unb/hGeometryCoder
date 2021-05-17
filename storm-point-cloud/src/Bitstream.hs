{-# LANGUAGE OverloadedStrings #-}
module Bitstream where

import qualified Data.ByteString as B
import Data.Word
import Data.Bits
import Data.List
import Data.Utils

buildEDX :: (Bin, PointCloudSize, Axis) -> Either String Bin
buildEDX (b, s, a) = writeEDX b s a

writeEDX :: Bin -> PointCloudSize -> Axis -> Either String Bin
writeEDX _ 0 _ = Left "Size without proper size!"
writeEDX [] _ _ = Left "Encoding failed!"
writeEDX bin size axis = do
    let (padding, dataBin) = writeEDXData [] bin
    headerBin <- writeEDXHeader 2 (fromIntegral size :: Word16) (combinePaddingWithAxis padding axis)
    Right $ headerBin ++ dataBin

addPadding :: NumByte -> Bin -> Either String Bin
addPadding nBytes bits
    | difference == 0 = Right bits
    | difference < 0  = Left "Bit array is bigger than NumByte size!"
    | otherwise       = Right $ padding ++ bits
        where nBits = nBytes * 8
              bitsSize = Prelude.length bits
              difference = nBits - bitsSize
              padding = replicate difference 0

integral2BitList :: (FiniteBits b) => b -> Bin
integral2BitList x = reverse $ map (f . testBit x) [0..(finiteBitSize x - 1)]
    where f y = if y then 1 else 0

bitListWithPadding :: (FiniteBits b) => NumByte -> b -> Either String Bin
bitListWithPadding nBytes number = do
    binary <- addPadding nBytes (integral2BitList number)
    Right $ transcodeSide nBytes binary

writeEDXHeader :: (FiniteBits b) => NumByte -> b -> LateralInfo -> Either String Bin
writeEDXHeader nBytes size padAxis = do
    binary <- bitListWithPadding nBytes size
    Right $ padAxis : binary

writeEDXData :: [Byte] -> Bin -> (Padding, Bin)
writeEDXData tcoded bin
    | binSize >= 8   = writeEDXData (tcoded ++ [transcode bin]) (drop 8 bin)
    | binSize == 0   = (0, tcoded)
    | otherwise      = (fromIntegral padding, finishTranscode)
        where binSize = Prelude.length bin
              padding = 8 - binSize
              finishTranscode = tcoded ++ [B.foldl' (\ res b -> (res `shiftL` 1) + b) 0 (B.pack bin) `shiftL` padding]

transcodeSide :: NumByte -> Bin -> [Byte]
transcodeSide 0 _ = []
transcodeSide nBytes bin = transcode (take 8 bin) : transcodeSide (nBytes - 1) (drop 8 bin)

combinePaddingWithAxis :: Padding -> Axis -> LateralInfo
combinePaddingWithAxis padding axis = (padding `shiftL` 4) .|. axis2Bin axis

axis2Bin :: Axis -> Byte
axis2Bin X = 0
axis2Bin Y = 1
axis2Bin Z = 2

transcode :: Bin -> Byte
transcode (b1:b2:b3:b4:b5:b6:b7:b8:_) =
    (b1 `shiftL` 7) .|.
    (b2 `shiftL` 6) .|.
    (b3 `shiftL` 5) .|.
    (b4 `shiftL` 4) .|.
    (b5 `shiftL` 3) .|.
    (b6 `shiftL` 2) .|.
    (b7 `shiftL` 1) .|. b8