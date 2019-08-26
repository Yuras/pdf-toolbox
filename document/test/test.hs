{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Pdf.Core.Object
import Pdf.Core.Writer
import Pdf.Document

import qualified Data.HashMap.Strict as HashMap
import Control.Exception (bracket, finally)
import Control.Monad
import System.IO
import qualified System.IO.Streams as Streams
import System.Directory (getTemporaryDirectory, removeFile)

import Test.Hspec

main :: IO ()
main = hspec $ do
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
