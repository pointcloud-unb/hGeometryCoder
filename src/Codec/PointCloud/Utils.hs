{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Codec.PointCloud.Utils where

import Codec.PointCloud.Driver.PLY.Types

import Data.ByteString.Char8 (ByteString, unpack)
import Control.Applicative
import Control.Monad (join)
import Data.List (findIndex)
import Data.Word
import Data.Either
import Flat
import Data.Bits
import GHC.Generics (Generic)
import qualified Data.Set as S

type Range = (Int, Int)
type Label = ByteString
type PointCloudSize = Int
type Coordinate = Int
type Index = Int
type Bit = Word8
type Byte = Word8
type Bin = [Bit]
type Padding = Word8
type LateralInfo = Word8
type NumByte = Int
type Mask = [Bool]
type Left = [Bool]
type Root = [Bool]
type TriForceRoot = [Bool]

data Axis = X | Y | Z
  deriving (Show, Eq, Generic, Flat)

data Mode = Normal | Single
  deriving (Show, Eq, Generic, Flat)

integral2BitList :: (FiniteBits b) => b -> Bin
integral2BitList x = reverse $ map (f . testBit x) [0..(finiteBitSize x - 1)]
    where f y = if y then 1 else 0

computeNBits :: Int -> Int
computeNBits n = round (logBase 2 (fromIntegral n :: Float)) :: Int

computePower2 :: Int -> Int
computePower2 n = head $ dropWhile (< n) [ 2^i | i <- [0..]]
