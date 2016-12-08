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
    pdf <- document
    infoDict <- do
      i <- documentInfo pdf
      case i of
        Just info -> return info
        Nothing -> error "Unimplemented: PDF document without Info dictionary"
    let Document xref tr = pdf
        Info infoRef info = infoDict
    fileSize <- getRIS >>= size
    let startxref =
          case xref of
            XRefTable off -> off
            XRefStream off _ -> off
        newInfo = setValueForKey "Title" (OStr $ Str $ fromString title) info
        newTr = setValueForKey "Prev" (ONumber $ NumInt $ fromIntegral startxref) tr
    is <- do
      ris <- getRIS
      seek ris 0
      inputStream ris
    liftIO $ Streams.withFileAsOutput output $ \ostream -> do
      Streams.supply is ostream
      runPdfWriter ostream $ do
        writeObject infoRef (ODict newInfo)
        writeXRefTable fileSize newTr
  print res
