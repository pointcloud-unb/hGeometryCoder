{-# LANGUAGE DeriveGeneric, DeriveAnyClass#-}

module Codec.PointCloud.Driver.PLY.Types where

import Flat
import GHC.Generics (Generic)
import Control.DeepSeq

import Data.ByteString (ByteString)
import Data.Int (Int8, Int16)
import Data.Word (Word8, Word16, Word32)


-- PLY representation --
data PLY = PLY { plyHeader :: !Header
               , plyData   :: DataBlocks }
  deriving (Eq, Show, Generic, Flat, NFData)

type DataBlocks = [DataLine]
type DataLine = [Scalar]


data Header = Header { hFormat :: !Format
                     , hElems  :: ![Element] }
  deriving (Eq, Show, Generic, Flat, NFData)

data Format = ASCII      -- ^ ASCII
            | BinaryLE   -- ^ Binary Little Endian
            | BinaryBE   -- ^ Binary Big Endian
            deriving (Eq, Show, Generic, Flat, NFData)

data Element = Element { elName  :: !ByteString
                       , elNum   :: !Int
                       , elProps :: ![Property] }
             deriving (Eq, Show, Generic, Flat, NFData)

data Property = ScalarProperty { sPropType :: !ScalarType
                               , sPropName :: !ByteString }
              | ListProperty { lPropIndexType :: !ScalarType
                             , lPropType      :: !ScalarType
                             , lPropName      :: !ByteString }
              deriving (Eq, Show, Generic, Flat, NFData)

data ScalarType = CharT
                | UcharT
                | ShortT
                | UshortT
                | IntT
                | UintT
                | FloatT
                | DoubleT
                deriving (Eq, Show, Generic, Flat, NFData)

data Scalar = CharS   {-# UNPACK #-} !Int8
            | UcharS  {-# UNPACK #-} !Word8
            | ShortS  {-# UNPACK #-} !Int16
            | UshortS {-# UNPACK #-} !Word16
            | IntS    {-# UNPACK #-} !Int
            | UintS   {-# UNPACK #-} !Word32
            | FloatS  {-# UNPACK #-} !Float
            | DoubleS {-# UNPACK #-} !Double
            deriving (Eq, Show, Generic, Flat, NFData)

