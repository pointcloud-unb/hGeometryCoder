
module Main where

import Codec.PointCloud.Driver.PLY.Parser (ply, ply')

import Criterion.Main
import Criterion.Types

import Data.Attoparsec.Char8 (parseOnly)
import qualified Data.ByteString as B (readFile)


myConfig = defaultConfig {
  -- Output HTML file
  reportFile = Just "parser-bench.html"
  -- Each benchmark may run for at most 2 minutes.
  , timeLimit = 120
  }

myEnv = do
  dustDense5 <- B.readFile "assets/dustDense5.ply" -- 17952 points
  dustDense6 <- B.readFile "assets/dustDense6.ply" -- 144554 points
  ricardo9 <- B.readFile "assets/ricardo9_frame0000.ply" -- 214656 points
  ricardo10 <- B.readFile "assets/ricardo10_frame0000.ply" -- 960703 points
  return (dustDense5, dustDense6, ricardo9, ricardo10)

-- Our benchmark harness.
main = do
  defaultMainWith myConfig [
    env myEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
        bgroup "ply/parser"
        [ bench "dustDense5.ply" $ nf (parseOnly ply) dustDense5
        , bench "dustDense6.ply" $ nf (parseOnly ply) dustDense6
        , bench "ricardo9.ply" $ nf (parseOnly ply) ricardo9
        , bench "ricardo10.ply" $ nf (parseOnly ply) ricardo10
        ],
    env myEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
      bgroup "ply/parser-sequence"
        [ bench "dustDense5.ply" $ nf (parseOnly ply') dustDense5
        , bench "dustDense6.ply" $ nf (parseOnly ply') dustDense6
        , bench "ricardo9.ply" $ nf (parseOnly ply') ricardo9
        , bench "ricardo10.ply" $ nf (parseOnly ply') ricardo10
        ]
        
    ]
