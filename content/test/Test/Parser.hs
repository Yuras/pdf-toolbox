{-# LANGUAGE OverloadedStrings #-}

module Test.Parser
(
  spec,
)
where

import Pdf.Core
import Pdf.Content.Parser
import Pdf.Content.Ops

import qualified Data.Attoparsec.ByteString as Parser
import qualified System.IO.Streams as Streams
import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
  parseContentSpec
  readNextOperatorSpec

parseContentSpec :: Spec
parseContentSpec = describe "parseContent" $ do
  it "should parse an object" $ do
    let input = "(hello)"
        res = Parser.parseOnly parseContent input
    res `shouldBe` Right (Just $ Obj $ String "hello")

  it "should parse an operator" $ do
    let input = "T*"
        res = Parser.parseOnly parseContent input
    res `shouldBe` Right (Just $ Op Op_T_star)

  it "should return Nothing when input is empty" $ do
    let input = ""
        res = Parser.parseOnly parseContent input
    res `shouldBe` Right Nothing

  it "should ignore comments at the beginning" $ do
    let input = "% comments\nTj"
        res = Parser.parseOnly parseContent input
    res `shouldBe` Right (Just $ Op Op_Tj)

  it "should ignore multiple lines of comments at the beginning" $ do
    let input = "% comments\nTj"
        res = Parser.parseOnly parseContent input
    res `shouldBe` Right (Just $ Op Op_Tj)

readNextOperatorSpec :: Spec
readNextOperatorSpec = describe "readNextOperator" $ do
  it "should return Nothing when there is no input" $ do
    let input = []
    is <- Streams.fromList input
    next <- readNextOperator is
    next `shouldBe` Nothing

  it "should collect all objects until an operator" $ do
    let input = [Obj o1, Obj o2, Op op]
        o1 = String "o1"
        o2 = String "o2"
        op = Op_Tj
    is <- Streams.fromList input
    next <- readNextOperator is
    next `shouldBe` Just (op, [o1, o2])

  it "should throw when there is no operator after arguments" $ do
    let input = [Obj o1, Obj o2]
        o1 = String "o1"
        o2 = String "o2"
    is <- Streams.fromList input
    readNextOperator is
      `shouldThrow` \Corrupted{} -> True
