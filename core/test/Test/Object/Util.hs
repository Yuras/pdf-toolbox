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
  streamValueSpec

boolValueSpec :: Spec
boolValueSpec = describe "boolValue" $ do
  it "should convert boolean value to Bool" $ do
    boolValue (Bool True) `shouldBe` Just True

  it "should return Nothing for other values" $ do
    boolValue (String "hello") `shouldBe` Nothing

stringValueSpec :: Spec
stringValueSpec = describe "stringValue" $ do
  it "should convert string value to ByteString" $ do
    stringValue (String "hello") `shouldBe` Just "hello"

  it "should return Nothing for other values" $ do
    stringValue (Bool True) `shouldBe` Nothing

intValueSpec :: Spec
intValueSpec = describe "intValue" $ do
  it "should convert int value to Int" $ do
    intValue (Number 42) `shouldBe` Just 42

  it "should not convert float value" $ do
    intValue (Number 42.6) `shouldBe` Nothing

  it "should not convert any other value" $ do
    intValue (Bool True) `shouldBe` Nothing

realValueSpec :: Spec
realValueSpec = describe "realValue" $ do
  it "should convert int value to Float" $ do
    realValue (Number 42) `shouldBe` Just 42.0

  it "should convert float value to Float" $ do
    realValue (Number 42.4) `shouldBe` Just 42.4

  it "should not convert any other value" $ do
    realValue (Bool True) `shouldBe` Nothing

arrayValueSpec :: Spec
arrayValueSpec = describe "arrayValue" $ do
  it "should convert array value to Array" $ do
    let arr = Vector.fromList [Bool True]
    arrayValue (Array arr) `shouldBe` Just arr

  it "should return Nothing for any other value" $ do
    arrayValue (Bool True) `shouldBe` Nothing

dictValueSpec :: Spec
dictValueSpec = describe "dictValue" $ do
  it "should convert dict value to Dict" $ do
    let dict = HashMap.fromList [("hello", Bool True)]
    dictValue (Dict dict) `shouldBe` Just dict

  it "should return Nothing for any other value" $ do
    dictValue (Bool True) `shouldBe` Nothing

nameValueSpec :: Spec
nameValueSpec = describe "nameValue" $ do
  it "should convert name value to Name" $ do
    nameValue (Name "hello") `shouldBe` Just "hello"

  it "should return Nothing for any other value" $ do
    nameValue (Bool True) `shouldBe` Nothing

refValueSpec :: Spec
refValueSpec = describe "refValue" $ do
  it "should convert ref value to Ref" $ do
    let ref = R 42 24
    refValue (Ref ref) `shouldBe` Just ref

  it "should return Nothing for any other value" $ do
    refValue (Bool True) `shouldBe` Nothing

streamValueSpec :: Spec
streamValueSpec = describe "streamValue" $ do
  it "should convert stream value to Stream" $ do
    let stream = S dict ("hello" :: String)
        dict = HashMap.fromList [("a", String "b")]
    streamValue (Stream stream) `shouldBe` Just stream

  it "should return Nothing for any other value" $ do
    streamValue (Bool True) `shouldBe` (Nothing :: Maybe (Stream String))
