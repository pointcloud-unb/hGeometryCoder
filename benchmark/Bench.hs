{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

module Main where

import Codec.PointCloud.Driver.PLY.Types
import Codec.PointCloud.Driver.PLY.Parser (parsePLY, parsePLY', unflatPLY)
import Codec.PointCloud.Driver.PLY.Output (writePLY, putPLY, flatPLY, writeFlatPLY)
import Data.Either

import GHC.Generics (Generic)
import Control.DeepSeq
import Flat (DecodeException(..))

import Criterion.Main
import Criterion.Types

import qualified Data.ByteString as B (readFile)

--deriving instance Generic DecodeException
--deriving instance NFData DecodeException


myConfig = defaultConfig {
  -- Output HTML file
  reportFile = Just "benchmark/results/report.html"
  -- Each benchmark may run for at most 1 minute.
  , timeLimit = 60
  }

plyLoadEnv = do
  dustDense5 <- B.readFile "assets/dustDense5.ply" -- 17952 points
  dustDense6 <- B.readFile "assets/dustDense6.ply" -- 144554 points
  ricardo9 <- B.readFile "assets/ricardo9_frame0000.ply" -- 214656 points
  ricardo10 <- B.readFile "assets/ricardo10_frame0000.ply" -- 960703 points
  return (dustDense5, dustDense6, ricardo9, ricardo10)


plyLoadParseEnv = do
  dustDense5 <- parsePLY <$> B.readFile "assets/dustDense5.ply" -- 17952 points
  dustDense6 <- parsePLY <$> B.readFile "assets/dustDense6.ply" -- 144554 points
  ricardo9 <- parsePLY <$> B.readFile "assets/ricardo9_frame0000.ply" -- 214656 points
  ricardo10 <- parsePLY <$> B.readFile "assets/ricardo10_frame0000.ply" -- 960703 points
  return (dustDense5, dustDense6, ricardo9, ricardo10)

plyLoadFlatEnv = do
  dustDense5 <- do
    dustDense5' <- parsePLY <$> B.readFile "assets/dustDense5.ply" -- 17952 points
    writeFlatPLY "benchmark/results/dustDense5.ply.flat" $ fromRight undefined dustDense5'
    B.readFile "benchmark/results/dustDense5.ply.flat"
  
  dustDense6 <- do
    dustDense6' <- parsePLY <$> B.readFile "assets/dustDense6.ply" -- 144554 points
    writeFlatPLY "benchmark/results/dustDense6.ply.flat" $ fromRight undefined dustDense6'
    B.readFile "benchmark/results/dustDense6.ply.flat"

  ricardo9 <- do
    ricardo9' <- parsePLY <$> B.readFile "assets/ricardo9_frame0000.ply" -- 214656 points
    writeFlatPLY "benchmark/results/ricardo9.ply.flat" $ fromRight undefined ricardo9' 
    B.readFile "benchmark/results/ricardo9.ply.flat"

  ricardo10 <- do
    ricardo10' <- parsePLY <$> B.readFile "assets/ricardo10_frame0000.ply" -- 960703 points
    writeFlatPLY "benchmark/results/ricardo10.ply.flat" $ fromRight undefined ricardo10'
    B.readFile "benchmark/results/ricardo10.ply.flat"

  return (dustDense5, dustDense6, ricardo9, ricardo10)




-- Our benchmark harness.
main = do
  defaultMainWith myConfig [
    -- Benchmark parsing.
    env plyLoadEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
        bgroup "ply/parsePLY"
        [ bench "dustDense5.ply" $ nf parsePLY dustDense5
        , bench "dustDense6.ply" $ nf parsePLY  dustDense6
        , bench "ricardo9.ply" $ nf parsePLY ricardo9
        , bench "ricardo10.ply" $ nf parsePLY ricardo10
--        , bench "seq-dustDense5.ply" $ nf parsePLY' dustDense5
--        , bench "seq-dustDense6.ply" $ nf parsePLY' dustDense6
--        , bench "seq-ricardo9.ply" $ nf parsePLY' ricardo9
--        , bench "seq-ricardo10.ply" $ nf parsePLY' ricardo10
        ]
    -- Benchmark saving PLY to disk
    , env plyLoadParseEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
        bgroup "ply/writePLY"
        [ bench "dustDense5.ply" $ nfIO $
          writePLY "benchmark/results/dustDense5.ply" $ fromRight undefined dustDense5
        , bench "dustDense6.ply" $ nfIO $
          writePLY "benchmark/results/dustDense6.ply" $ fromRight undefined dustDense6
        , bench "ricardo9_frame0000.ply" $ nfIO $
          writePLY "benchmark/results/ricardo9_frame0000.ply" $ fromRight undefined ricardo9
        , bench "ricardo10_frame0000.ply" $ nfIO $
          writePLY "benchmark/results/ricardo10_frame0000.ply" $ fromRight undefined ricardo10
        ]
    -- Benchmark PLY bytestring creation
    , env plyLoadParseEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
        bgroup "ply/putPLY"
        [ bench "dustDense5.ply" $ nf (\x -> putPLY <$> x) dustDense5
        , bench "dustDense6.ply" $ nf (\x -> putPLY <$> x) dustDense6
        , bench "ricardo9_frame0000.ply" $ nf (\x -> putPLY <$> x) ricardo9
        , bench "ricardo10_frame0000.ply" $ nf (\x -> putPLY <$> x) ricardo10
        ]
    -- Benchmark saving flat PLY to disk
    , env plyLoadParseEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
        bgroup "ply/writeFlatPLY"
        [ bench "dustDense5.ply" $ nfIO $
          writeFlatPLY "benchmark/results/dustDense5.ply.flat" $ fromRight undefined dustDense5
        , bench "dustDense6.ply" $ nfIO $
          writeFlatPLY "benchmark/results/dustDense6.ply.flat" $ fromRight undefined dustDense6
        , bench "ricardo9_frame0000.ply" $ nfIO $
          writeFlatPLY "benchmark/results/ricardo9_frame0000.ply.flat" $ fromRight undefined ricardo9
        , bench "ricardo10_frame0000.ply" $ nfIO $
          writeFlatPLY "benchmark/results/ricardo10_frame0000.ply.flat" $ fromRight undefined ricardo10
        ]
    -- Benchmark PLY flating
    , env plyLoadParseEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
        bgroup "ply/flatPLY"
        [ bench "dustDense5.ply" $ nf (\x -> flatPLY <$> x) dustDense5
        , bench "dustDense6.ply" $ nf (\x -> flatPLY <$> x) dustDense6
        , bench "ricardo9_frame0000.ply" $ nf (\x -> flatPLY <$> x) ricardo9
        , bench "ricardo10_frame0000.ply" $ nf (\x -> flatPLY <$> x) ricardo10
        ]
    -- Benchmark load flat PLY
    , env plyLoadFlatEnv $ \ ~(dustDense5, dustDense6, ricardo9, ricardo10) ->
        bgroup "ply/unflatPLY"
        [ bench "dustDense5.ply" $ nf (fromRight undefined . unflatPLY) dustDense5
        , bench "dustDense6.ply" $ nf (fromRight undefined . unflatPLY) dustDense6
        , bench "ricardo9_frame0000.ply" $ nf (fromRight undefined . unflatPLY) ricardo9
        , bench "ricardo10_frame0000.ply" $ nf (fromRight undefined . unflatPLY)  ricardo10
        ]

    ]
    

