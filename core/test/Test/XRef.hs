{-# LANGUAGE OverloadedStrings #-}

module Test.XRef
(
  spec
)
where

import qualified Data.ByteString as ByteString
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import qualified System.IO.Streams as Streams

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.XRef
import Pdf.Toolbox.Core.IO.Buffer
import Pdf.Toolbox.Core.Exception

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
      buf <- bytesToBuffer "helloxref\nworld"
      readXRef buf 5
      ) `shouldReturn` XRefTable 5

    it "should support xref stream" $ (do
      buf <- bytesToBuffer "hello1 1 obj\n<<>>stream\r\ncontent"
      readXRef buf 5
      ) `shouldReturn` XRefStream 5 (S HashMap.empty 25)

    it "should throw exception if xref not found" $ (do
      buf <- bytesToBuffer "hello\n"
      readXRef buf 0
      ) `shouldThrow` anyException


  describe "lastXRef" $ do
    it "should find the latest xref" $ (
      bytesToBuffer "helloxref\nxref\nstartxref\n10\n%%EOF\
        \worldstartxref\n5\n%%EOF"
      >>= lastXRef
      ) `shouldReturn` XRefTable 5

    it "should throw Corrupted when xref not found" $ (
      bytesToBuffer "helloxref\n%%EOF"
      >>= lastXRef
      ) `shouldThrow` \Corrupted{} -> True


  describe "trailer" $ do
    it "should return the dictionary for xref stream" $
      let dict = HashMap.fromList [("Hello", OStr "World")]
      in trailer undefined (XRefStream 0 (S dict 0))
        `shouldReturn` dict

    it "should parse trailer after xref table" $ (do
      buf <- bytesToBuffer "helloxref\n1 1\n0000000001 00000 n\r\n\
        \trailer\n<</Hello(world)>>"
      trailer buf (XRefTable 5)
      ) `shouldReturn` HashMap.fromList [("Hello", OStr "world")]

    it "should handle multisection table" $ (do
      buf <- bytesToBuffer "helloxref\n1 1\n0000000001 00000 n\r\n\
        \1 1\n0000000002 00000 n\r\ntrailer\n<</Hello(world)>>"
      trailer buf (XRefTable 5)
      ) `shouldReturn` HashMap.fromList [("Hello", OStr "world")]

    it "should throw Corrupted exception if can't parse" $ (do
      buf <- bytesToBuffer "helloxref\n1 Hello(world)>>"
      trailer buf (XRefTable 5)
      ) `shouldThrow` \Corrupted{} -> True


  describe "prevXRef" $ do
    it "should read xref located at offset from\
        \ Prev entry in current trailer" $ (do
      let dict = HashMap.fromList [("Prev", ONumber 5)]
      buf <- bytesToBuffer "helloxref\n"
      prevXRef buf (XRefStream undefined (S dict undefined))
      ) `shouldReturn` Just (XRefTable 5)

    it "should return Nothing for the last xref" $ (do
      let dict = HashMap.fromList []
      buf <- bytesToBuffer "helloxref\n"
      prevXRef buf (XRefStream undefined (S dict undefined))
      ) `shouldReturn` Nothing

    it "should throw Corrupted when Prev is not an int" $ (do
      let dict = HashMap.fromList [("Prev", OStr "hello")]
      buf <- bytesToBuffer "helloxref\n"
      prevXRef buf (XRefStream undefined (S dict undefined))
      ) `shouldThrow` \Corrupted{} -> True

  describe "lookupTableEntry" $ do
    it "should look for the entry in subsections" $ (do
      buf <- bytesToBuffer "helloxref\n\
        \1 2\n\
        \0000000011 00000 n\r\n\
        \0000000022 00000 n\r\n\
        \3 2\n\
        \0000000033 00000 n\r\n\
        \0000000044 00000 n\r\n\
        \trailer"
      lookupTableEntry buf (XRefTable 5) (R 4 0)
      ) `shouldReturn` Just (TableEntry 44 0 False)

    it "should return Nothing when not found" $ (do
      buf <- bytesToBuffer "helloxref\n\
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
          [ ("Index", OArray $ Vector.fromList $ map ONumber [3, 4])
          , ("W", OArray $ Vector.fromList $ map ONumber [1, 2, 1])
          , ("Size", ONumber 4)
          ]
    it "should handle free objects" $ (do
      is <- Streams.fromByteString bytes
      lookupStreamEntry (S dict is) (R 6 0)
      ) `shouldReturn` Just (StreamEntryFree 4 0)

    it "should handle used objects" $ (do
      is <- Streams.fromByteString bytes
      lookupStreamEntry (S dict is) (R 4 0)
      ) `shouldReturn` Just (StreamEntryUsed 2 3)

    it "should handle compressed objects" $ (do
      is <- Streams.fromByteString bytes
      lookupStreamEntry (S dict is) (R 5 0)
      ) `shouldReturn` Just (StreamEntryCompressed 3 4)

    it "should return Nothing when object to found" $ (do
      is <- Streams.fromByteString bytes
      lookupStreamEntry (S dict is) (R 7 0)
      ) `shouldReturn` Nothing

    it "should handle multiple sections" $ (do
      let dict' = HashMap.fromList
            [ ("Index", OArray $ Vector.fromList $ map ONumber [3, 2, 10, 2])
            , ("W", OArray $ Vector.fromList $ map ONumber [1, 2, 1])
            , ("Size", ONumber 4)
            ]
      is <- Streams.fromByteString bytes
      lookupStreamEntry (S dict' is) (R 11 0)
      ) `shouldReturn` Just (StreamEntryFree 4 0)
