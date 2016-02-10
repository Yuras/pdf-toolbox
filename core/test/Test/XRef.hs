{-# LANGUAGE OverloadedStrings #-}

module Test.XRef
( spec
)
where

import Pdf.Core.Object
import Pdf.Core.XRef
import Pdf.Core.Exception
import qualified Pdf.Core.IO.Buffer as Buffer

import qualified Data.ByteString as ByteString
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import qualified System.IO.Streams as Streams
import Test.Hspec

spec :: Spec
spec = describe "XRef" $ do
  describe "isTable" $ do
    it "should return True when the stream starts from \"xref\\n\" string" $
      (Streams.fromByteString "xref\n" >>= isTable)
        `shouldReturn` True

    it "should return False when the stream doesn't start from \"xref\\n\"" $
      (Streams.fromByteString "_xref\n" >>= isTable)
        `shouldReturn` False

    it "should consume \"xref\\n\" prefix" $ (do
      is <- Streams.fromByteString "xref\nhello"
      void $ isTable is
      Streams.readExactly 5 is
      ) `shouldReturn` "hello"


  describe "readXRef" $ do
    it "should support xref table" $ (do
      buf <- Buffer.fromBytes "helloxref\nworld"
      readXRef buf 5
      ) `shouldReturn` XRefTable 5

    it "should support xref stream" $ (do
      buf <- Buffer.fromBytes "hello1 1 obj\n<<>>stream\r\ncontent"
      readXRef buf 5
      ) `shouldReturn` XRefStream 5 (S HashMap.empty 25)

    it "should throw exception if xref not found" $ (do
      buf <- Buffer.fromBytes "hello\n"
      readXRef buf 0
      ) `shouldThrow` anyException


  describe "lastXRef" $ do
    it "should find the latest xref" $ (
      Buffer.fromBytes "helloxref\nxref\nstartxref\n10\n%%EOF\
        \worldstartxref\n5\n%%EOF"
      >>= lastXRef
      ) `shouldReturn` XRefTable 5

    it "should throw Corrupted when xref not found" $ (
      Buffer.fromBytes "helloxref\n%%EOF"
      >>= lastXRef
      ) `shouldThrow` \Corrupted{} -> True


  describe "trailer" $ do
    it "should return the dictionary for xref stream" $
      let dict = HashMap.fromList [("Hello", String "World")]
      in trailer undefined (XRefStream 0 (S dict 0))
        `shouldReturn` dict

    it "should parse trailer after xref table" $ (do
      buf <- Buffer.fromBytes "helloxref\n1 1\n0000000001 00000 n\r\n\
        \trailer\n<</Hello(world)>>"
      trailer buf (XRefTable 5)
      ) `shouldReturn` HashMap.fromList [("Hello", String "world")]

    it "should handle multisection table" $ (do
      buf <- Buffer.fromBytes "helloxref\n1 1\n0000000001 00000 n\r\n\
        \1 1\n0000000002 00000 n\r\ntrailer\n<</Hello(world)>>"
      trailer buf (XRefTable 5)
      ) `shouldReturn` HashMap.fromList [("Hello", String "world")]

    it "should throw Corrupted exception if can't parse" $ (do
      buf <- Buffer.fromBytes "helloxref\n1 Hello(world)>>"
      trailer buf (XRefTable 5)
      ) `shouldThrow` \Corrupted{} -> True


  describe "prevXRef" $ do
    it "should read xref located at offset from\
        \ Prev entry in current trailer" $ (do
      let dict = HashMap.fromList [("Prev", Number 5)]
      buf <- Buffer.fromBytes "helloxref\n"
      prevXRef buf (XRefStream undefined (S dict undefined))
      ) `shouldReturn` Just (XRefTable 5)

    it "should return Nothing for the last xref" $ (do
      let dict = HashMap.fromList []
      buf <- Buffer.fromBytes "helloxref\n"
      prevXRef buf (XRefStream undefined (S dict undefined))
      ) `shouldReturn` Nothing

    it "should throw Corrupted when Prev is not an int" $ (do
      let dict = HashMap.fromList [("Prev", String "hello")]
      buf <- Buffer.fromBytes "helloxref\n"
      prevXRef buf (XRefStream undefined (S dict undefined))
      ) `shouldThrow` \Corrupted{} -> True

  describe "lookupTableEntry" $ do
    it "should look for the entry in subsections" $ (do
      buf <- Buffer.fromBytes "helloxref\n\
        \1 2\n\
        \0000000011 00000 n\r\n\
        \0000000022 00000 n\r\n\
        \3 2\n\
        \0000000033 00000 n\r\n\
        \0000000044 00000 n\r\n\
        \trailer"
      lookupTableEntry buf (XRefTable 5) (R 4 0)
      ) `shouldReturn` Just (EntryUsed 44 0)

    it "should return free entry" $ (do
      buf <- Buffer.fromBytes "helloxref\n\
        \1 2\n\
        \0000000011 00000 n\r\n\
        \0000000022 00001 f\r\n\
        \trailer"
      lookupTableEntry buf (XRefTable 5) (R 2 0)
      ) `shouldReturn` Just (EntryFree 22 0)

    it "should return Nothing when not found" $ (do
      buf <- Buffer.fromBytes "helloxref\n\
        \1 2\n\
        \0000000011 00000 n\r\n\
        \0000000022 00000 n\r\n\
        \trailer"
      lookupTableEntry buf (XRefTable 5) (R 4 0)
      ) `shouldReturn` Nothing

  describe "lookupStreamEntry" $ do
    let bytes = ByteString.pack
          [ 0,  0, 1,  2
          , 1,  0, 2,  3
          , 2,  0, 3,  4
          , 0,  0, 4,  0
          ]
        dict = HashMap.fromList
          [ ("Index", Array $ Vector.fromList $ map Number [3, 4])
          , ("W", Array $ Vector.fromList $ map Number [1, 2, 1])
          , ("Size", Number 4)
          ]
    it "should handle free objects" $ (do
      is <- Streams.fromByteString bytes
      lookupStreamEntry dict is (R 6 0)
      ) `shouldReturn` Just (EntryFree 4 0)

    it "should handle used objects" $ (do
      is <- Streams.fromByteString bytes
      lookupStreamEntry dict is (R 4 0)
      ) `shouldReturn` Just (EntryUsed 2 3)

    it "should handle compressed objects" $ (do
      is <- Streams.fromByteString bytes
      lookupStreamEntry dict is (R 5 0)
      ) `shouldReturn` Just (EntryCompressed 3 4)

    it "should return Nothing when object to found" $ (do
      is <- Streams.fromByteString bytes
      lookupStreamEntry dict is (R 7 0)
      ) `shouldReturn` Nothing

    it "should handle multiple sections" $ (do
      let dict' = HashMap.fromList
            [ ("Index", Array $ Vector.fromList $ map Number [3, 2, 10, 2])
            , ("W", Array $ Vector.fromList $ map Number [1, 2, 1])
            , ("Size", Number 4)
            ]
      is <- Streams.fromByteString bytes
      lookupStreamEntry dict' is (R 11 0)
      ) `shouldReturn` Just (EntryFree 4 0)
