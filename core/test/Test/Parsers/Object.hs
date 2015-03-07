{-# LANGUAGE OverloadedStrings #-}

module Test.Parsers.Object
(
  spec
)
where

import Data.Attoparsec.ByteString

import Pdf.Toolbox.Core.Parsers.Object

import Test.Hspec

spec :: Spec
spec = describe "Parsers.Object" $ do
  parseStringSpec
  parseHexStringSpec
  parseBoolSpec

parseStringSpec :: Spec
parseStringSpec = describe "parseString" $ do
  it "should unescape 3-digit character" $ do
    parseOnly parseString "(hello\\040world)"
      `shouldBe` Right "hello world"

  it "should unescape 2-digit character" $ do
    parseOnly parseString "(hello\\40world)"
      `shouldBe` Right "hello world"

  it "should unescape 1-digit character" $ do
    parseOnly parseString "(hello\\0world)"
      `shouldBe` Right "hello\NULworld"

  it "should accept nested parens" $ do
    parseOnly parseString "(hello( )world)"
      `shouldBe` Right "hello( )world"

  it "should unescape special chars" $ do
    parseOnly parseString "(\\(\\)\\\\\\n\\f\\r\\t\\b)"
      `shouldBe` Right "()\\\n\f\r\t\b"

parseHexStringSpec :: Spec
parseHexStringSpec = describe "parseHexString" $ do
  it "should parse hex string" $ do
    parseOnly parseHexString "<00FFff>"
      `shouldBe` Right "\NUL\255\255"

parseBoolSpec :: Spec
parseBoolSpec = describe "parseBool" $ do
  it "should parse 'true' as True" $ do
    parseOnly parseBool "true"
      `shouldBe` Right True

  it "should parse 'false' as False" $ do
    parseOnly parseBool "false"
      `shouldBe` Right False
