{-# LANGUAGE OverloadedStrings #-}

-- | Change PDF document title
--
-- The example shows how to use incremental updates to change PDF file

module Main
(
  main
)
where

import Pdf.Core.Object
import Pdf.Core.Object.Util
import Pdf.Core.XRef
import Pdf.Core.Stream (knownFilters)
import Pdf.Core.Util
import Pdf.Core.Exception
import qualified Pdf.Core.IO.Buffer as Buffer
import qualified Pdf.Core.File as File
import Pdf.Core.Writer
import Pdf.Document

-- Using the internals to switch from 'pdf-toolbox-document' level
-- to 'pdf-toolbox-core'
import Pdf.Document.Internal.Types

import Data.String
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import System.IO
import qualified System.IO.Streams as Streams
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 3) $ do
    mapM_ putStrLn [
      "Usage:",
      "\t./change-document-title input.pdf \"New Title\" output.pdf"
      ]
    error "Please supply 3 arguments"
  let (input, title, output) = case args of
        [a, b, c] -> (a, b, c)
        _ -> error "expected 3 arbuments"

  withBinaryFile input ReadMode $ \h -> do
    buf <- Buffer.fromHandle h
    file <- File.fromBuffer knownFilters buf
    pdf <- fromFile file
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

    nextFree <- sure $ (HashMap.lookup "Size" tr >>= intValue)
      `notice` "Trailer Size should be an integer"

    let hasTable = case xref of
          XRefTable{} -> True
          XRefStream{} -> False

    let startxref =
          case xref of
            XRefTable off -> off
            XRefStream off _ -> off
        newInfo = HashMap.insert "Title" (String $ fromString title) info
        newTr
          = HashMap.insert "Prev" (Number $ fromIntegral startxref)
          . HashMap.insert "Size" (Number (fromIntegral size))
          $ tr
        size = if hasTable then nextFree else succ nextFree

    Buffer.seek buf 0
    let is = Buffer.toInputStream buf

    Streams.withFileAsOutput output $ \ostream -> do
      Streams.supply is ostream
      writer <- makeWriter ostream
      writeObject writer infoRef (Dict newInfo)
      if hasTable
        then writeXRefTable writer fileSize newTr
        else writeXRefStream writer fileSize (R nextFree 0) newTr
