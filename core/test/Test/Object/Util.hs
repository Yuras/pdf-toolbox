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
  intValueSpec
  realValueSpec

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

intValueSpec :: Spec
intValueSpec = describe "intValue" $ do
  it "should convert int value to Int" $ do
    intValue (ONumber 42) `shouldBe` Just 42

  it "should not convert float value" $ do
    intValue (ONumber 42.6) `shouldBe` Nothing

  it "should not convert any other value" $ do
    intValue (OBoolean True) `shouldBe` Nothing

realValueSpec :: Spec
realValueSpec = describe "realValue" $ do
  it "should convert int value to Float" $ do
    realValue (ONumber 42) `shouldBe` Just 42.0

  it "should convert float value to Float" $ do
    realValue (ONumber 42.4) `shouldBe` Just 42.4

  it "should not convert any other value" $ do
    realValue (OBoolean True) `shouldBe` Nothing
