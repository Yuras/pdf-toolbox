{-# LANGUAGE OverloadedStrings #-}

module Test.Object.Util
(
  spec,
)
where

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Util

import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Test.Hspec

spec :: Spec
spec = describe "Object.Util" $ do
  boolValueSpec
  stringValueSpec
  intValueSpec
  realValueSpec
  arrayValueSpec
  dictValueSpec
  nameValueSpec
  refValueSpec

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

arrayValueSpec :: Spec
arrayValueSpec = describe "arrayValue" $ do
  it "should convert array value to Array" $ do
    let arr = Vector.fromList [OBoolean True]
    arrayValue (OArray arr) `shouldBe` Just arr

  it "should return Nothing for any other value" $ do
    arrayValue (OBoolean True) `shouldBe` Nothing

dictValueSpec :: Spec
dictValueSpec = describe "dictValue" $ do
  it "should convert dict value to Dict" $ do
    let dict = HashMap.fromList [("hello", OBoolean True)]
    dictValue (ODict dict) `shouldBe` Just dict

  it "should return Nothing for any other value" $ do
    dictValue (OBoolean True) `shouldBe` Nothing

nameValueSpec :: Spec
nameValueSpec = describe "nameValue" $ do
  it "should convert name value to Name" $ do
    nameValue (OName "hello") `shouldBe` Just "hello"

  it "should return Nothing for any other value" $ do
    nameValue (OBoolean True) `shouldBe` Nothing

refValueSpec :: Spec
refValueSpec = describe "refValue" $ do
  it "should convert ref value to Ref" $ do
    let ref = R 42 24
    refValue (ORef ref) `shouldBe` Just ref

  it "should return Nothing for any other value" $ do
    refValue (OBoolean True) `shouldBe` Nothing
