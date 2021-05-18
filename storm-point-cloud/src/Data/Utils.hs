{-# LANGUAGE OverloadedStrings #-}
module Data.Utils where
  
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Input.Types
import Control.Applicative
import Control.Monad (join)
import Data.List (findIndex)
import Data.Word
import Data.Either
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

data Axis = X | Y | Z
  deriving (Show, Eq)



filterFromLabel :: Label -> PLY -> Either String PLY
filterFromLabel label (PLY h d) = do
                        (before, current, element) <- getUntilLabel (hElems h) label 0
                        return $ PLY (Header (hFormat h) [element]) $ take current $ drop before d

getUntilLabel :: [Element] -> Label -> Int -> Either String (Int, Int, Element)
getUntilLabel [] l _ = Left $ "Didn't find label " ++ unpack l ++ " in getUntilLabel"
getUntilLabel (e:es) l c
  | elName e == l = Right (c, elNum e, e)
  | otherwise     = getUntilLabel es l (c + elNum e)

computeNBits :: Int -> Int
computeNBits n = round (logBase 2 (fromIntegral n :: Float)) :: Int

computePower2 :: Int -> Int
computePower2 n = head $ dropWhile (< n) [ 2^i | i <- [0..]]

getCoordinatesIndexes :: Header -> Either String (Index, Index, Index)
getCoordinatesIndexes (Header _ els) = do
                                      props <- findPropsFromLabel els "vertex"
                                      (,,) <$> props `fromLabel` "x" <*> props `fromLabel` "y" <*> props `fromLabel` "z"

findPropsFromLabel :: [Element] -> Label -> Either String [Property]
findPropsFromLabel [] l = Left $ "Didn't find label " ++ unpack l ++ " in findPropsFromLabel"
findPropsFromLabel (e:es) l
  | elName e == l = Right $ elProps e
  | otherwise     = findPropsFromLabel es l

fromLabel :: [Property] -> Label -> Either String Int
fromLabel ps label = maybe (Left errorMsg) Right mIndex
  where mIndex = findIndex (\p -> sPropName p == label) ps
        errorMsg = "Didn't find label " ++ unpack label ++ "in fromLabel"
