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

boolValueSpec :: Spec
boolValueSpec = describe "boolValue" $ do
  it "should convert boolean value to Bool" $ do
    boolValue (OBoolean True) `shouldBe` Just True

  it "should return Nothing for other values" $ do
    boolValue (OStr $ Str "hello") `shouldBe` Nothing
