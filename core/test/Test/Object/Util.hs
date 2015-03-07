{-# LANGUAGE OverloadedStrings #-}

module Test.Object.Util
(
  spec,
)
where

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Util

import Test.Hspec

spec :: Spec
spec = describe "Object.Util" $ do
  boolValueSpec
  stringValueSpec

boolValueSpec :: Spec
boolValueSpec = describe "boolValue" $ do
  it "should convert boolean value to Bool" $ do
    boolValue (OBoolean True) `shouldBe` Just True

  it "should return Nothing for other values" $ do
    boolValue (OStr "hello") `shouldBe` Nothing

stringValueSpec :: Spec
stringValueSpec = describe "stringValue" $ do
  it "should convert string value to ByteString" $ do
    stringValue (OStr "hello") `shouldBe` Just "hello"

  it "should return Nothing for other values" $ do
    stringValue (OBoolean True) `shouldBe` Nothing
