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
    formatSpec
    propertySpec

formatSpec = do
  describe "formatLine" $ do
    describe "ASCII" $ do
      it "Proper line" $
        ("format ascii 1.0" :: B.ByteString) ~> format `shouldParse` ASCII
      it "Spaces in the middle of line" $
        ("format   ascii   1.0   " :: B.ByteString) ~> format `shouldParse` ASCII
      it "Tabs in the middle of line" $
        ("format\tascii\t1.0" :: B.ByteString) ~> format `shouldParse` ASCII
    describe "Little_Endian" $ do
      it "Proper line" $
        ("format binary_little_endian 1.0" :: B.ByteString) ~> format `shouldParse` BinaryLE
      it "Spaces in the middle of line" $
        ("format   binary_little_endian   1.0   " :: B.ByteString) ~> format `shouldParse` BinaryLE
      it "Tabs in the middle of line" $
        ("format\tbinary_little_endian\t1.0" :: B.ByteString) ~> format `shouldParse` BinaryLE
    describe "Big_Endian" $ do
      it "Proper line" $
        ("format binary_big_endian 1.0" :: B.ByteString) ~> format `shouldParse` BinaryBE
      it "Spaces in the middle of line" $
        ("format   binary_big_endian   1.0   " :: B.ByteString) ~> format `shouldParse` BinaryBE
      it "Tabs in the middle of line" $
        ("format\tbinary_big_endian\t1.0" :: B.ByteString) ~> format `shouldParse` BinaryBE

propertySpec = do
  describe "property" $ do
    describe "ScalarProperty" $ do
      it "" $ pending
    describe "ScalarProperty" $ do
      it "" $ pending
