{-# LANGUAGE OverloadedStrings #-}

module Test.Name
(
  spec
)
where

import qualified Pdf.Toolbox.Core.Name as Name

import Data.Either
import Test.Hspec

spec :: Spec
spec = describe "Name" $ do
  makeSpec

makeSpec :: Spec
makeSpec = describe "make" $ do
  it "should wrap bytestring to name" $ do
    Name.make "hello" `shouldSatisfy` isRight

  it "should not allow 0 byte inside a name" $ do
    Name.make "hello\0" `shouldSatisfy` isLeft
