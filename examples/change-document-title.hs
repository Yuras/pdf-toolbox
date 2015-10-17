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
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import System.IO
import qualified System.IO.Streams as Streams
import System.Environment

import Pdf.Toolbox.Core
import qualified Pdf.Toolbox.Core.IO.Buffer as Buffer
import Pdf.Toolbox.Document
import qualified Pdf.Toolbox.Document.File as File

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

  withBinaryFile input ReadMode $ \h -> do
    buf <- Buffer.fromHandle h
    file <- File.withBuffer knownFilters buf
    pdf <- pdfWithFile file
    doc <- document pdf

    infoDict <- do
      i <- documentInfo doc
      case i of
        Just info -> return info
        Nothing -> error "Unimplemented: PDF document without Info dictionary"

    let Document _ tr = doc
        Info _ infoRef info = infoDict

    fileSize <- Buffer.size buf
    xref <- lastXRef buf

    let startxref =
          case xref of
            XRefTable off -> off
            XRefStream off _ -> off
        newInfo = HashMap.insert "Title" (String $ fromString title) info
        newTr = HashMap.insert "Prev" (Number$ fromIntegral startxref) tr

    Buffer.seek buf 0
    let is = Buffer.toInputStream buf

    Streams.withFileAsOutput output $ \ostream -> do
      Streams.supply is ostream
      runPdfWriter ostream $ do
        writeObject infoRef (Dict newInfo)
        writeXRefTable fileSize newTr
