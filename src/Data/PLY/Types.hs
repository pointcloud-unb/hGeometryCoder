module Data.PLY.Types where

import Data.ByteString (ByteString)
import Data.Int
import Data.Word


data Header = Header Format [Element]
  deriving (Show)

data Format = ASCII      -- ^ ASCII
            | BinaryLE   -- ^ Binary Little Endian
            | BinaryBE   -- ^ Binary Big Endian
            deriving (Show)

data Element = Element { elName  :: !ByteString
                       , elNum   :: !Int
                       , elProps :: ![Property] }
             deriving (Show)

data Property = ScalarProperty { sPropType :: !ScalarType
                               , sPropName :: !ByteString }
              | ListProperty { lPropIndexType :: !ScalarType
                             , lPropType      :: !ScalarType
                             , lPropName      :: !ByteString }
              deriving(Show)

data ScalarType = CharT
                | UcharT
                | ShortT
                | UshortT
                | IntT
                | UintT
                | FloatT
                | DoubleT
                deriving (Eq, Show)

data Scalar = CharS Int8
            | UcharS Word8
            | ShortS Int16
            | UshortS Word16
            | IntS Int
            | UintS Word32
            | FloatS Float
            | DoubleS Double
            deriving (Show)
