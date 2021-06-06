
module Main where

import Data.Input.PLY

import Criterion.Main
import Criterion.Types

import qualified Data.ByteString as B (readFile)


myConfig = defaultConfig {
  -- Output HTML file
  reportFile = Just "parser-bench.html"
  -- Each benchmark may run for at most 2 minutes.
  , timeLimit = 120
  }

myEnv = do
  ddd32 <- B.readFile "test_files/DDD32.ply" -- 17952 points
  ddd64 <- B.readFile "test_files/DDD64.ply" -- 144554 points
  return (ddd32, ddd64)

-- Our benchmark harness.
main = do
  defaultMainWith myConfig [
    env myEnv $ \ ~(ddd32, ddd64) ->
        bgroup "ply/parser"
        [ bench "DDD32.ply" $ nf parsePLY ddd32
        , bench "DDD64.ply" $ nf parsePLY ddd64
        ]
    ]
