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
      it "property char x" $
        ("property char x" :: B.ByteString) ~> property `shouldParse` ScalarProperty CharT "x"
      it "property char x - spaces in between" $
        ("  property   char   x  " :: B.ByteString) ~> property `shouldParse` ScalarProperty CharT "x"
      it "property char x - tabs in between" $
        ("\tproperty\t char\t x\t" :: B.ByteString) ~> property `shouldParse` ScalarProperty CharT "x"
      it "property int8 x" $
        ("property int8 x" :: B.ByteString) ~> property `shouldParse` ScalarProperty CharT "x"
      it "property uchar x" $
        ("property uchar x" :: B.ByteString) ~> property `shouldParse` ScalarProperty UcharT "x"
      it "property uint8 x" $
        ("property uint8 x" :: B.ByteString) ~> property `shouldParse` ScalarProperty UcharT "x"
      it "property short x" $
        ("property short x" :: B.ByteString) ~> property `shouldParse` ScalarProperty ShortT "x"
      it "property int16 x" $
        ("property int16 x" :: B.ByteString) ~> property `shouldParse` ScalarProperty ShortT "x"
      it "property ushort x" $
        ("property ushort x" :: B.ByteString) ~> property `shouldParse` ScalarProperty UshortT "x"
      it "property uint16 x" $
        ("property uint16 x" :: B.ByteString) ~> property `shouldParse` ScalarProperty UshortT "x"
      it "property int x" $
        ("property int x" :: B.ByteString) ~> property `shouldParse` ScalarProperty IntT "x"
      it "property int32 x" $
        ("property int32 x" :: B.ByteString) ~> property `shouldParse` ScalarProperty IntT "x"
      it "property uint x" $
        ("property uint x" :: B.ByteString) ~> property `shouldParse` ScalarProperty UintT "x"
      it "property uint32 x" $
        ("property uint32 x" :: B.ByteString) ~> property `shouldParse` ScalarProperty UintT "x"
      it "property float x" $
        ("property float x" :: B.ByteString) ~> property `shouldParse` ScalarProperty FloatT "x"
      it "property float32 x" $
        ("property float32 x" :: B.ByteString) ~> property `shouldParse` ScalarProperty FloatT "x"
      it "property double x" $
        ("property double x" :: B.ByteString) ~> property `shouldParse` ScalarProperty DoubleT "x"
      it "property float64 x" $
        ("property float64 x" :: B.ByteString) ~> property `shouldParse` ScalarProperty DoubleT "x"
    describe "ScalarProperty" $ do
      it "property list char char x" $
        ("property list char char x" :: B.ByteString) ~> property `shouldParse`
        ListProperty CharT CharT "x"
      it "property char x - spaces in between" $
        ("  property list   char   char   x  " :: B.ByteString) ~> property `shouldParse`
        ListProperty CharT CharT "x"
      it "property char x - tabs in between" $
        ("\tproperty list\tchar\tchar\tx\t" :: B.ByteString) ~> property `shouldParse`
        ListProperty CharT CharT "x"
      it "\"property list\" should be treated as keyword - no spaces between words" $
        property `shouldFailOn` ("property   list char char x" :: B.ByteString)
      it "\"property list\" should be treated as keyword - no tabs between words" $
        property `shouldFailOn` ("property\tlist char char x" :: B.ByteString)

