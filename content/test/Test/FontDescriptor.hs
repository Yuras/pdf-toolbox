{-# LANGUAGE OverloadedStrings #-}

module Test.FontDescriptor
(
  spec,
)
where

import Pdf.Content.FontDescriptor

import Data.Int
import Test.Hspec

spec :: Spec
spec = describe "FontDescriptor" $ do
  testFlagSet

defFD :: FontDescriptor
defFD = FontDescriptor
  { fdFontName = "test"
  , fdFontFamily = Nothing
  , fdFontStretch = Nothing
  , fdFontWeight = Nothing
  , fdFlags = 5
  , fdFontBBox = Nothing
  , fdItalicAngle = -12.5
  , fdDescent = Nothing
  , fdAscent = Nothing
  , fdLeading = Nothing
  , fdCapHeight = Nothing
  , fdXHeight = Nothing
  , fdStemV = Nothing
  , fdStemH = Nothing
  , fdAvgWidth = Nothing
  , fdMaxWidth = Nothing
  , fdMissingWidth = Nothing
  , fdCharSet = Nothing
  }

testFlagSet :: Spec
testFlagSet = describe "flagSet" $ do
  it "should return True when testing FixedPitch on 5" $ do
    let res = flagSet defFD FixedPitch
    res `shouldSatisfy` (==True)

  it "should return False when testing Serif on 5" $ do
    let res = flagSet defFD Serif
    res `shouldSatisfy` (==False)

  it "should return True when testing Symbolic on 5" $ do
    let res = flagSet defFD Symbolic
    res `shouldSatisfy` (==True)

  it "should return False when testing Script on 5" $ do
    let res = flagSet defFD Script
    res `shouldSatisfy` (==False)

  it "should return False when testing ForceBold on 5" $ do
    let res = flagSet defFD ForceBold
    res `shouldSatisfy` (==False)

  it "should return True when testing ForceBold on (2^32)-1" $ do
    let res = flagSet (defFD {fdFlags = ((2^32)::Int64)-1})  ForceBold
    res `shouldSatisfy` (==True)

  it "should return False when testing ForceBold on 0" $ do
    let res = flagSet (defFD {fdFlags = 0}) ForceBold
    res `shouldSatisfy` (==False)

