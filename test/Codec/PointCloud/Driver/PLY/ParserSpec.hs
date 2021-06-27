{-# LANGUAGE OverloadedStrings #-}

module Codec.PointCloud.Driver.PLY.ParserSpec where

import Codec.PointCloud.Driver.PLY.Parser
import Codec.PointCloud.Driver.PLY.Types

import qualified Data.ByteString as B 

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "Parser" $ do
    describe "formatLine" $ do
      it "Proper ASCII line" $ do
        ("format ascii 1.0" :: B.ByteString) ~> format `shouldParse` ASCII
