{-# LANGUAGE OverloadedStrings #-}

-- | Change PDF document title
--
-- The example shows how to use incremental updates to change PDF file

module Main
(
  main
)
where

import Data.String
import Data.ByteString.Lazy.Builder
import Control.Monad
import System.IO
import qualified System.IO.Streams as Streams
import System.Environment

import Pdf.Toolbox.Core
import Pdf.Toolbox.Document

-- Using the internals to switch from 'pdf-toolbox-document' level
-- to 'pdf-toolbox-core'
import Pdf.Toolbox.Document.Internal.Types

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 3) $ do
    mapM_ putStrLn [
      "Usage:",
      "\t./change-document-title input.pdf \"New Title\" output.pdf"
      ]
    error "Please supply 3 arguments"
  let [input, title, output] = args
  res <- withBinaryFile input ReadMode $ \handle ->
    runPdfWithHandle handle knownFilters $ do
    Document xref tr <- document
    infoRef <- lookupDict "Info" tr >>= fromObject
    info <- lookupObject infoRef >>= fromObject
    fileSize <- getRIS >>= size
    let startxref =
          case xref of
            XRefTable off -> off
            XRefStream off _ -> off
        newInfo = setValueForKey "Title" (OStr $ Str $ fromString title) info
        newTr = setValueForKey "Prev" (ONumber $ NumInt $ fromIntegral startxref) tr
        patch = buildPatch [(Just $ ODict newInfo, infoRef)] fileSize newTr
    is <- do
      ris <- getRIS
      seek ris 0
      inputStream ris
    liftIO $ Streams.withFileAsOutput output WriteMode (BlockBuffering Nothing) $ \ostream -> do
      Streams.connect is ostream
      Streams.writeLazyByteString (toLazyByteString patch) ostream
  print res
