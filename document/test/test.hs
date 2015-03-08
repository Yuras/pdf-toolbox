{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import qualified Data.HashMap.Strict as HashMap
import Control.Exception (bracket, finally)
import Control.Monad
import System.IO
import qualified System.IO.Streams as Streams
import System.Directory (getTemporaryDirectory, removeFile)

import Pdf.Toolbox.Core
import Pdf.Toolbox.Document

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "simple.pdf" $ do
    it "should have title" $
      withSimpleFile $ \h -> do
      pdf <- pdfWithHandle h
      doc <- document pdf
      maybe_info <- documentInfo doc
      title <-
        case maybe_info of
          Nothing -> return Nothing
          Just info -> infoTitle info
      title `shouldBe` Just "simple PDF file"

-- | Generate simple PDF file for tests
withSimpleFile :: (Handle -> IO ()) -> IO ()
withSimpleFile action = do
  dir <- getTemporaryDirectory
  bracket
    (openBinaryTempFile dir "simple.pdf")
    (\(path, h) -> hClose h `finally` removeFile path)
    $ \(_, h) -> do
  out <- Streams.handleToOutputStream h
  runPdfWriter out $ do
    writePdfHeader
    deleteObject (Ref 0 1) 65535
    forM_ objects $ \(ref, obj) ->
      writeObject ref obj
    writeXRefTable 0 tr
  action h
  where
  tr = HashMap.fromList
    [ ("Size", ONumber $ fromIntegral $ length objects + 1)
    , ("Info", ORef infoRef)
    ]
  info = HashMap.fromList
    [ ("Title", OStr "simple PDF file")
    ]
  objects =
    [ (infoRef, ODict info)
    ]
  infoRef = Ref 1 0
