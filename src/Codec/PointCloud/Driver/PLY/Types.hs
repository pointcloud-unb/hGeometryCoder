{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module Codec.PointCloud.Driver.PLY.Types where

import Flat
import GHC.Generics (Generic)
import Control.DeepSeq

import Data.ByteString (ByteString)
import Data.Int (Int8, Int16)
import Data.Word (Word8, Word16, Word32)

import qualified Data.Sequence as S
import qualified Data.Vector as V



-- PLY representation --
data PLY = PLY { plyHeader :: !Header
               , plyData   :: DataBlocks }
  deriving (Show, Generic, Flat, NFData, Eq)

type DataBlocks = [DataLine]
type DataLine = [Scalar]

-- PLY using Sequence -- 
data PLY' = PLY' { plyHeader' :: !Header
                 , plyData'   :: DataBlocks' }
  deriving (Show, Generic, NFData, Eq)

type DataBlocks' = S.Seq DataLine'
type DataLine' = S.Seq Scalar

-- PLY using Vector -- 
data PLYV = PLYV { plyHeaderV :: !Header
                 , plyDataV   :: DataBlocksV }
  deriving (Show, Generic, NFData, Eq)

type DataBlocksV = V.Vector DataLineV
type DataLineV = V.Vector Scalar



data Header = Header { hFormat :: !Format
                     , hElems  :: ![Element] }
  deriving (Show, Generic, Flat, NFData, Eq)

data Format = ASCII      -- ^ ASCII
            | BinaryLE   -- ^ Binary Little Endian
            | BinaryBE   -- ^ Binary Big Endian
            deriving (Show, Generic, Flat, NFData, Eq)

data Element = Element { elName  :: !ByteString
                       , elNum   :: !Int
                       , elProps :: ![Property] }
             deriving (Show, Generic, Flat, NFData, Eq)

data Property = ScalarProperty { sPropType :: !ScalarType
                               , sPropName :: !ByteString }
              | ListProperty { lPropIndexType :: !ScalarType
                             , lPropType      :: !ScalarType
                             , lPropName      :: !ByteString }
              deriving(Show, Generic, Flat, NFData, Eq)

data ScalarType = CharT
                | UcharT
                | ShortT
                | UshortT
                | IntT
                | UintT
                | FloatT
                | DoubleT
                deriving (Eq, Show, Generic, Flat, NFData)

data Scalar = CharS   !Int8
            | UcharS  !Word8
            | ShortS  !Int16
            | UshortS !Word16
            | IntS    !Int
            | UintS   !Word32
            | FloatS  !Float
            | DoubleS !Double
            deriving (Show, Generic, Flat, NFData, Eq)
