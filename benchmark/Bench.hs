{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

module Main where

import Codec.PointCloud.Driver.PLY.Types
import Codec.PointCloud.Driver.PLY.Parser (parsePLY, readPLY, readFlatPLY, unflatPLY)
import Codec.PointCloud.Driver.PLY.Output (writePLY, putPLY, flatPLY, writeFlatPLY)
import Data.Either
import Control.Monad
import Control.Applicative

import GHC.Generics (Generic)
import Control.DeepSeq
import Flat (DecodeException(..))

import Criterion.Main
import Criterion.Types

import qualified Data.ByteString as B (readFile)


assetNames = 
  ( "dustDense5.ply"  -- 17952 points
  , "dustDense6.ply"  -- 144554 points
  , "ricardo9.ply"    -- 214656 points
  , "ricardo10.ply"   -- 960703 points
  )


emptyPLY = PLY
  { plyHeader = Header ASCII []
  , plyData = []}


myConfig = defaultConfig {
  -- Output HTML file
  reportFile = Just "benchmark/results/report.html"
  -- Each benchmark may run for at most 1 minute.
  , timeLimit = 60
  }



-- Our benchmark harness.
main = do
  defaultMainWith myConfig [
    bgroup "PLY" [
      bgroup "Input" [ parsePLYBench
                     , readPLYBench
                     , unflatPLYBench
                     , readFlatPLYBench
                     ]
      , bgroup "Output" [ putPLYBench
                        , writePLYBench
                        , flatPLYBench
                        , writeFlatPLYBench
                        ]
      ]
    ]



-- PLY input benchmarks --
plyLoadEnv = do
 dustDense5 <- B.readFile "assets/dustDense5.ply"
 dustDense6 <- B.readFile "assets/dustDense6.ply"
 ricardo9 <- B.readFile "assets/ricardo9.ply"
 ricardo10 <- B.readFile "assets/ricardo10.ply"
 return (dustDense5, dustDense6, ricardo9, ricardo10)

parsePLYBench = env plyLoadEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
  bgroup "parsePLY"
  [ bench "dustDense5.ply" $ nf parsePLY dustDense5
--  , bench "dustDense6.ply" $ nf parsePLY dustDense6
--  , bench "ricardo9.ply"   $ nf parsePLY ricardo9
  , bench "ricardo10.ply"  $ nf parsePLY ricardo10
  ]

readPLYBench = 
  bgroup "readPLY"
  [ bench "dustDense5.ply" $ nfAppIO readPLY "assets/dustDense5.ply"
--  , bench "dustDense6.ply" $ nfAppIO readPLY "assets/dustDense6.ply"
--  , bench "ricardo9.ply"   $ nfAppIO readPLY "assets/ricardo9.ply"
  , bench "ricardo10.ply"  $ nfAppIO readPLY "assets/ricardo10.ply"
  ]



plyLoadFlatEnv = do
  dustDense5 <- do
    dustDense5' <- fromRight emptyPLY . parsePLY <$> B.readFile "assets/dustDense5.ply" 
    writeFlatPLY "benchmark/results/dustDense5.ply.flat" dustDense5'
    B.readFile "benchmark/results/dustDense5.ply.flat"
  
  dustDense6 <- do
    dustDense6' <- fromRight emptyPLY . parsePLY <$> B.readFile "assets/dustDense6.ply"
    writeFlatPLY "benchmark/results/dustDense6.ply.flat" dustDense6'
    B.readFile "benchmark/results/dustDense6.ply.flat"

  ricardo9 <- do
    ricardo9' <- fromRight emptyPLY . parsePLY <$> B.readFile "assets/ricardo9.ply"
    writeFlatPLY "benchmark/results/ricardo9.ply.flat" ricardo9' 
    B.readFile "benchmark/results/ricardo9.ply.flat"

  ricardo10 <- do
    ricardo10' <- fromRight emptyPLY . parsePLY <$> B.readFile "assets/ricardo10.ply"
    writeFlatPLY "benchmark/results/ricardo10.ply.flat" ricardo10'
    B.readFile "benchmark/results/ricardo10.ply.flat"

  return (dustDense5, dustDense6, ricardo9, ricardo10)


unflatPLYBench =
  let benchFunction = (fromRight emptyPLY . unflatPLY)
  in
    env plyLoadFlatEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
    bgroup "unflatPLY"
    [ bench "dustDense5.ply" $ nf benchFunction dustDense5
    -- , bench "dustDense6.ply" $ nf benchFunction dustDense6
    -- , bench "ricardo9.ply" $ nf benchFunction ricardo9
    , bench "ricardo10.ply" $ nf benchFunction  ricardo10
    ]

readFlatPLYBench =
  let benchFunction file = fromRight emptyPLY <$> readFlatPLY file
  in
    env plyLoadFlatEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
    bgroup "readFlatPLY"
    [ bench "dustDense5.ply" $ nfAppIO benchFunction "benchmark/results/dustDense5.ply.flat"
    -- , bench "dustDense6.ply" $ nfAppIO benchFunction "benchmark/results/dustDense6.ply.flat"
    -- , bench "ricardo9.ply"   $ nfAppIO benchFunction "benchmark/results/ricardo9.ply.flat"
    , bench "ricardo10.ply"  $ nfAppIO benchFunction "benchmark/results/ricardo10.ply.flat"
    ]


-- PLY output benchmarks -- 
plyLoadParseEnv = do
  dustDense5 <- fromRight emptyPLY . parsePLY <$> B.readFile "assets/dustDense5.ply"
  dustDense6 <- fromRight emptyPLY . parsePLY <$> B.readFile "assets/dustDense6.ply"
  ricardo9 <- fromRight emptyPLY . parsePLY <$> B.readFile "assets/ricardo9.ply"
  ricardo10 <- fromRight emptyPLY . parsePLY <$> B.readFile "assets/ricardo10.ply"
  return (dustDense5, dustDense6, ricardo9, ricardo10)

putPLYBench =
  let benchFunction = putPLY
  in
    env plyLoadParseEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
    bgroup "putPLY"
    [ bench "dustDense5.ply" $ nf benchFunction dustDense5
    -- , bench "dustDense6.ply" $ nf benchFunction dustDense6
    -- , bench "ricardo9.ply"   $ nf benchFunction ricardo9
    , bench "ricardo10.ply"  $ nf benchFunction ricardo10
    ]

writePLYBench =
  let benchFunction = writePLY
  in
    env plyLoadParseEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
    bgroup "writePLY"
    [ bench "dustDense5.ply" $ nfAppIO (benchFunction "benchmark/results/dustDense5.ply") dustDense5
      -- , bench "dustDense6.ply" $ nfAppIO (benchFunction "benchmark/results/dustDens6.ply") dustDense6
      -- , bench "ricardo9.ply"   $ nfAppIO (benchFunction "benchmark/results/ricardo9.ply") ricardo9
    , bench "ricardo10.ply"  $ nfAppIO (benchFunction "benchmark/results/ricardo10.ply") ricardo10
    ]

flatPLYBench =
  let benchFunction = flatPLY
  in
    env plyLoadParseEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
    bgroup "flatPLY"
    [ bench "dustDense5.ply" $ nf benchFunction dustDense5
      -- , bench "dustDense6.ply" $ nf benchFunction dustDense6
      -- , bench "ricardo9.ply"   $ nfbenchFunction ricardo9
    , bench "ricardo10.ply"  $ nf benchFunction ricardo10
    ]

writeFlatPLYBench =
  let benchFunction = writeFlatPLY
  in
    env plyLoadParseEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
    bgroup "writeFlatPLY"
    [ bench "dustDense5.ply" $ nfAppIO (benchFunction "benchmark/results/dustDense5.ply") dustDense5
      -- , bench "dustDense6.ply" $ nfAppIO (benchFunction "benchmark/results/dustDens6.ply") dustDense6
      -- , bench "ricardo9.ply"   $ nfAppIO (benchFunction "benchmark/results/ricardo9.ply") ricardo9
    , bench "ricardo10.ply"  $ nfAppIO (benchFunction "benchmark/results/ricardo10.ply") ricardo10
    ]


