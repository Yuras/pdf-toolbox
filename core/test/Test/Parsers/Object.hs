{-# LANGUAGE OverloadedStrings #-}

module Test.Parsers.Object
(
  spec
)
where

import Data.Attoparsec.ByteString

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Parsers.Object

import Test.Hspec

spec :: Spec
spec = describe "Parsers.Object" $ do
  parseStrSpec
  parseBoolSpec

parseStrSpec :: Spec
parseStrSpec = describe "parseStr" $ do
  it "should unescape 3-digit character" $ do
    parseOnly parseStr "(hello\\040world)"
      `shouldBe` Right (Str "hello world")

  it "should unescape 2-digit character" $ do
    parseOnly parseStr "(hello\\40world)"
      `shouldBe` Right (Str "hello world")

  it "should unescape 1-digit character" $ do
    parseOnly parseStr "(hello\\0world)"
      `shouldBe` Right (Str "hello\NULworld")

parseBoolSpec :: Spec
parseBoolSpec = describe "parseBool" $ do
  it "should parse 'true' as True" $ do
    parseOnly parseBool "true"
      `shouldBe` Right True

  it "should parse 'false' as False" $ do
    parseOnly parseBool "false"
      `shouldBe` Right False
