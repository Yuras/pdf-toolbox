{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Pdf.Core.Object
import Pdf.Core.Writer
import Pdf.Document

import qualified Test.Internal.Util as Internal.Util

import qualified Data.HashMap.Strict as HashMap
import Control.Exception (bracket, finally)
import Control.Monad
import System.IO
import qualified System.IO.Streams as Streams
import System.Directory (getTemporaryDirectory, removeFile)
import System.Timeout

import Test.Hspec

main :: IO ()
main = hspec $ do
  Internal.Util.spec

  describe "simple.pdf" $ do
    it "should have title" $
      withSimpleFile $ \h -> do
        pdf <- fromHandle h
        doc <- document pdf
        maybe_info <- documentInfo doc
        title <-
          case maybe_info of
            Nothing -> return Nothing
            Just info -> infoTitle info
        title `shouldBe` Just "simple PDF file"

  describe "nested_xobject" $ do
    it "should have correct text" $ do
      withPdfFile "test/files/nested_xobject.pdf" $ \pdf -> do
        doc <- document pdf
        catalog <- documentCatalog doc
        root <- catalogPageNode catalog
        page <- pageNodePageByNum root 0
        -- here timeout breaks infinite loop in case of a bug
        txt <- timeout 5000000 $ pageExtractText page
        txt `shouldBe` Just
          "\nHello World!!!\nXObject is here\nnested XObject is here"

  describe "FontDescription with indirect fields" $ do
    it "should have correct text" $ do
      withPdfFile "test/files/indirect_font_desc_fields.pdf" $ \pdf -> do
        doc <- document pdf
        catalog <- documentCatalog doc
        root <- catalogPageNode catalog
        page <- pageNodePageByNum root 0
        -- here timeout breaks infinite loop in case of a bug
        txt <- timeout 5000000 $ pageExtractText page
        txt `shouldBe` Just
          "\nHello World!!!\nXObject is here\nnested XObject is here"

-- | Generate simple PDF file for tests
withSimpleFile :: (Handle -> IO ()) -> IO ()
withSimpleFile action = do
  dir <- getTemporaryDirectory
  bracket
    (openBinaryTempFile dir "simple.pdf")
    (\(path, h) -> hClose h `finally` removeFile path)
    $ \(_, h) -> do
      out <- Streams.handleToOutputStream h

      writer <- makeWriter out
      writeHeader writer
      deleteObject writer (R 0 1) 65535
      forM_ objects $ \(ref, obj) ->
        writeObject writer ref obj
      writeXRefTable writer 0 tr

      action h
  where
  tr = HashMap.fromList
    [ ("Size", Number $ fromIntegral $ length objects + 1)
    , ("Info", Ref infoRef)
    ]
  info = HashMap.fromList
    [ ("Title", String "simple PDF file")
    ]
  objects =
    [ (infoRef, Dict info)
    ]
  infoRef = R 1 0
