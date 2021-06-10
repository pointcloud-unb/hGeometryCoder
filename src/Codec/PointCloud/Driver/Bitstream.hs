{-# LANGUAGE OverloadedStrings #-}

module Codec.PointCloud.Driver.Bitstream where

import Codec.PointCloud.Utils
import Codec.PointCloud.Types.PointCloud
import Codec.PointCloud.Types.Voxel


import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S

import Data.ByteString.Builder
import Data.Char
import Data.Word
import Data.Bits
import Data.List

--fixedHeader :: PointCloudSize -> B.ByteString
--fixedHeader s = B.pack (fromIntegral s :: Word16)
fixedHeader :: Int -> [Char]
fixedHeader s = "ply\nformat ascii 1.0\nelement vertex " ++ show s ++ "\nproperty float x\nproperty float y\nproperty float z\nend_header\n"

buildPLY :: PointCloud -> Either String BC.ByteString
buildPLY (PointCloud sV _)=  Right $ BC.pack $ fixedHeader (S.size sV) ++ concatMap voxel2String sV

voxel2String :: Voxel -> String
voxel2String (Voxel u v w) = uB ++ " "  ++ vB ++ " " ++ wB ++ "\n"
    where uB = show u
          vB = show v
          wB = show w

buildEDX :: (Bin, PointCloudSize, Axis) -> Either String Bin
buildEDX (b, s, a) = writeEDX b s a

buildEDX' :: (Bin, PointCloudSize, Axis) -> Either String B.ByteString
buildEDX' (b, s, a) = writeEDX' b s a

writeEDX :: Bin -> PointCloudSize -> Axis -> Either String Bin
writeEDX _ 0 _ = Left "Size without proper size!"
writeEDX [] _ _ = Left "Encoding failed!"
writeEDX bin size axis = do
    let (padding, dataBin) = writeEDXData [] bin (Prelude.length bin)
    headerBin <- writeEDXHeader size (combinePaddingWithAxis padding axis)
    Right $ headerBin ++ dataBin

writeEDX' :: Bin -> PointCloudSize -> Axis -> Either String B.ByteString
writeEDX' _ 0 _ = Left "Size without proper size!"
writeEDX' [] _ _ = Left "Encoding failed!"
writeEDX' bin size axis = do
    let (padding, dataBin) = writeEDXData' mempty bin (Prelude.length bin)
    headerBin <- writeEDXHeader size (combinePaddingWithAxis padding axis)
    Right $ B.pack headerBin <> toLazyByteString dataBin


{- addPadding :: NumByte -> Bin -> Either String Bin
addPadding nBytes bits
    | difference == 0 = Right bits
    | difference < 0  = Left "Bit array is bigger than NumByte size!"
    | otherwise       = Right $ padding ++ bits
        where nBits = nBytes * 8
              bitsSize = Prelude.length bits
              difference = nBits - bitsSize
              padding = replicate difference 0

bitListWithPadding :: (FiniteBits b) => NumByte -> b -> Either String Bin
bitListWithPadding nBytes number = do
    binary <- addPadding nBytes (integral2BitList number)
    Right $ transcodeSide nBytes binary -}

integral2BitList :: (FiniteBits b) => b -> Bin
integral2BitList x = reverse $ map (f . testBit x) [0..(finiteBitSize x - 1)]
    where f y = if y then 1 else 0

writeEDXHeader :: PointCloudSize -> LateralInfo -> Either String Bin
writeEDXHeader size padAxis = Right [padAxis, transcode binary]
    where binary = integral2BitList (fromIntegral $ computeNBits size :: Word8)
    

writeEDXData :: [Byte] -> Bin -> Int -> (Padding, Bin)
writeEDXData tcoded bin binSize
    | binSize >= 8   = writeEDXData (tcoded ++ [transcode bin]) (drop 8 bin) (binSize - 8)
    | binSize == 0   = (0, tcoded)
    | otherwise      = (fromIntegral padding, finishTranscode)
        where --binSize = Prelude.length bin
              padding = 8 - binSize
              finishTranscode = tcoded ++ [B.foldl' (\ res b -> res `shiftL` 1 + b) 0 (B.pack bin) `shiftL` padding]

writeEDXData' :: Builder -> Bin -> Int -> (Padding, Builder)
writeEDXData' tcoded bin binSize
    | binSize >= 8   = writeEDXData' (tcoded <> word8 (transcode bin)) (drop 8 bin) (binSize - 8)
    | binSize == 0   = (0, tcoded)
    | otherwise      = (fromIntegral padding, finishTranscode)
        where --binSize = Prelude.length bin
              padding = 8 - binSize
              finishTranscode = tcoded <> word8 (B.foldl' (\ res b -> res `shiftL` 1 + b) 0 (B.pack bin) `shiftL` padding)

transcodeSide :: Bin -> [Byte]
transcodeSide bin = [transcode (take 8 bin), transcode (drop 8 bin)]

combinePaddingWithAxis :: Padding -> Axis -> LateralInfo
combinePaddingWithAxis padding axis = padding `shiftL` 4 .|. axis2Bin axis

axis2Bin :: Axis -> Byte
axis2Bin X = 0
axis2Bin Y = 1
axis2Bin Z = 2

transcode :: Bin -> Byte
transcode (b1:b2:b3:b4:b5:b6:b7:b8:_) =
    b1 `shiftL` 7 .|.
    b2 `shiftL` 6 .|.
    b3 `shiftL` 5 .|.
    b4 `shiftL` 4 .|.
    b5 `shiftL` 3 .|.
    b6 `shiftL` 2 .|.
    b7 `shiftL` 1 .|. b8

