{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Codec.PointCloud.Driver.PLY.Utils where

import Codec.PointCloud.Driver.PLY.Types
import Codec.PointCloud.Utils

import Data.ByteString.Char8 (unpack)


import Data.List (findIndex)

filterFromLabel :: Label -> PLY -> Either String PLY
filterFromLabel label (PLY h d) = do
                        (before, current, element) <- getUntilLabel (hElems h) label 0
                        return $ PLY (Header (hFormat h) [element]) $ take current $ drop before d

getUntilLabel :: [Element] -> Label -> Int -> Either String (Int, Int, Element)
getUntilLabel [] l _ = Left $ "Didn't find label " ++ unpack l ++ " in getUntilLabel"
getUntilLabel (e:es) l c
  | elName e == l = Right (c, elNum e, e)
  | otherwise     = getUntilLabel es l (c + elNum e)

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
