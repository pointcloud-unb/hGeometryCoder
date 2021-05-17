module Data.Input.DecoderParser where

import Control.Applicative
import qualified Data.ByteString as B
import Data.Structures.PCBitStream
import Data.Utils
import Data.Bits
-- Decoder parser
newtype Parser a = Parser { runParser :: B.ByteString -> Maybe (B.ByteString, a) }

instance Functor Parser where
  fmap f p =
    Parser $ \input -> do
      (input', a) <- runParser p input
      Just (input', f a)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  p1 <*> p2 =
    Parser $ \input -> do
      (input', f)  <- runParser p1 input
      (input'', a) <- runParser p2 input'
      return (input'', f a)

byteP :: Parser Byte
byteP = Parser f
    where
    f input | B.null input = Nothing
            | otherwise    = Just (rest, head)
        where
        head = B.head input
        rest = B.tail input

sizeP :: Parser (Byte, Byte)
sizeP = (,) <$> byteP <*> byteP

bin2Axis :: Byte -> Axis
bin2Axis 0 = X
bin2Axis 1 = Y
bin2Axis 2 = Z

-- First nible is padding and second one is axis
extractPadAxis :: Byte -> (Padding, Axis)
extractPadAxis b = (b `shiftR` 4, bin2Axis $ b .&. 0x0F)

-- Transform next 2 bytes to an int, i.e, size of pointcloud
extractPCSize :: (Byte, Byte) -> PointCloudSize
extractPCSize (b1, b2) = fComplete $ fShift ((0 :: Int) .|. fromIntegral b1)
    where fShift x = x `shiftL` 8 :: Int
          fComplete x = x .|. fromIntegral b2 :: Int

bitstreamPC :: B.ByteString -> Maybe PCBitStream
bitstreamPC input = do
    (input', padAxis) <- runParser byteP input
    (payload, pcSizeBytes) <- runParser sizeP input'
    let (padding, axis) = extractPadAxis padAxis
    let size = extractPCSize pcSizeBytes
    return $ PCBitStream padding axis size payload
    