
module Main where

import Data.Input.PLY

import Criterion.Main
import Criterion.Types

import qualified Data.ByteString as B (readFile)


myConfig = defaultConfig {
  -- Output HTML file
  reportFile = Just "parser.html"
  -- Increase time limit to 30 seconds
  , timeLimit = 30
  }

-- Our benchmark harness.
main = defaultMainWith myConfig [
  bgroup "ply/parser"
    [ bench "DDD32.ply"  $ nfIO (parsePLY <$> B.readFile "test_files/DDD32.ply")
    , bench "DDD64.ply"  $ nfIO (parsePLY <$> B.readFile "test_files/DDD64.ply")
    ]
  ]
