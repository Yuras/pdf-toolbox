{-# LANGUAGE OverloadedStrings #-}

module Test.Object.Builder
(
  spec,
)
where

import Pdf.Toolbox.Core.Object.Builder

import qualified Data.ByteString.Builder as Builder
import Test.Hspec

spec :: Spec
spec = describe "Object.Builder" $ do
  buildBoolSpec

buildBoolSpec :: Spec
buildBoolSpec = describe "buildBool" $ do
  it "should build 'true' for True" $ do
    let res = buildBool True 
    Builder.toLazyByteString res `shouldBe` "true"

  it "should build 'false' for False" $ do
    let res = buildBool False
    Builder.toLazyByteString res `shouldBe` "false"
