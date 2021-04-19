{-# LANGUAGE OverloadedStrings #-}
module Data.PLY
  (loadPLY
  , loadHeader
  , getProperties
  , plyHeader
  , plyData
  ) where

import qualified Data.ByteString as B (ByteString, readFile)
import Data.Attoparsec.ByteString.Char8 hiding (char)

import Data.PLY.Parser
import Data.PLY.Types

loadPLY :: FilePath -> IO (Either String PLY)
loadPLY f = do
          resultHeader <- loadHeader <$> B.readFile f
          case resultHeader of
            (Left msg) -> return (Left msg)
            (Right (Done r parsedHeader)) -> let dataParser = count (getCounter parsedHeader) (dataLine $ getProperties parsedHeader)
                                             in return $ fmap (PLY parsedHeader) (parseOnly dataParser r)
            (Right _) -> return (Left "Something crazy happenned")

loadHeader :: B.ByteString -> Either String (Result Header)
loadHeader = result . parse header
  where
    result (Fail _ ctxt msg) = Left $ "Parse failed: " ++ msg ++ " in " ++ show ctxt
    result (Partial _) = Left "Incomplete header..."
    result doneHeader = Right doneHeader

getProperties :: Header -> [Property]
getProperties (Header _ []) = []
getProperties (Header _ ((Element _ _ p):_)) = p

getCounter :: Header -> Int
getCounter (Header _ []) = 0
getCounter (Header _ ((Element _ c _):_)) = c

