{-# LANGUAGE OverloadedStrings #-}
module Data.Input.PLY
  ( parsePLY
  ) where


import qualified Data.ByteString as B (ByteString)
import Data.Attoparsec.ByteString.Char8 hiding (char)

import Data.Input.Parser
import Data.Input.Types
import Control.Monad (forM, join)

parsePLY :: B.ByteString -> Either String PLY
parsePLY b = do
          resultHeader <- loadHeader b
          case resultHeader of
            (Done r parsedHeader) ->
              let dataParser = forM (hElems parsedHeader) elementData
              in case parseOnly dataParser r of
                (Left _) -> Left "Invalid data"
                (Right x) -> return $ PLY parsedHeader (join x)
            _ -> Left "Something crazy happenned"

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

