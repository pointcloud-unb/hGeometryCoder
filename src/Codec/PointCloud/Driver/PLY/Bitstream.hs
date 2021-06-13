{-# LANGUAGE OverloadedStrings #-}

module Codec.PointCloud.Driver.PLY.Bitstream where

import Codec.PointCloud.Types.PointCloud
import Codec.PointCloud.Types.Voxel

import qualified Data.ByteString as B (ByteString)
import Data.Attoparsec.ByteString.Char8 hiding (char)

import Codec.PointCloud.Driver.PLY.Parser
import Codec.PointCloud.Driver.PLY.Types
import Control.Monad (forM, join)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Set as S

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

parsePLY :: B.ByteString -> Either String PLY
parsePLY  = parseOnly ply

-- DEPRECATED          
-- parsePLY :: B.ByteString -> Either String PLY
-- parsePLY b = do
--           resultHeader <- loadHeader b
--           case resultHeader of
--             (Done r parsedHeader) ->
--               let dataParser = forM (hElems parsedHeader) elementData
--               in case parseOnly dataParser r of 
--                 (Left _) -> Left "Invalid data"
--                 (Right x) -> return $ PLY parsedHeader (join x)
--             _ -> Left "Something crazy happenned"

-- loadHeader :: B.ByteString -> Either String (Result Header)
-- loadHeader = result . parse header
--   where
--       result (Fail _ ctxt msg) = Left $ "Parse failed: " ++ msg ++ " in " ++ show ctxt
--       result (Partial _) = Left "Incomplete header..."
--       result doneHeader = Right doneHeader

