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
  buildStringSpec

buildBoolSpec :: Spec
buildBoolSpec = describe "buildBool" $ do
  it "should build 'true' for True" $ do
    let res = buildBool True 
    Builder.toLazyByteString res `shouldBe` "true"

  it "should build 'false' for False" $ do
    let res = buildBool False
    Builder.toLazyByteString res `shouldBe` "false"

buildStringSpec :: Spec
buildStringSpec = describe "buildString" $ do
  it "should produce literal string when all chars are printable" $ do
    let res = buildString "hello"
    Builder.toLazyByteString res `shouldBe` "(hello)"

  it "should produce hex string when there are not printable chars" $ do
    let res = buildString "\NUL\255"
    Builder.toLazyByteString res `shouldBe` "<00ff>"

  it "should escape special chars" $ do
    let res = buildString "()\\"
    Builder.toLazyByteString res `shouldBe` "(\\(\\)\\\\)"
