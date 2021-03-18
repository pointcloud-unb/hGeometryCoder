module Data.PLY
  ( loadPLY
  , plyHeader
  , plyData
  ) where

import Data.Attoparsec.ByteString.Char8 (parse, IResult (..))
import qualified Data.ByteString as B (ByteString, readFile)

import Data.PLY.Parser
import Data.PLY.Types


loadPLY :: FilePath -> IO (Either String PLY)
loadPLY = fmap preloadPLY . B.readFile

preloadPLY :: B.ByteString -> Either String PLY
preloadPLY = result . parse header
  where
    result (Fail _ ctxt msg) = Left $ "Parse failed: " ++ msg ++ " in " ++ show ctxt
    result (Partial _) = Left $ "Incomplete header..."
    result (Done t r) = Right $ PLY r t
