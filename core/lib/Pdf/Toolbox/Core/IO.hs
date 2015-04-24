{-# LANGUAGE ScopedTypeVariables #-}

-- | Basic IO operations for PDF

module Pdf.Toolbox.Core.IO
(
  IS,
  RIS,
  RIS.fromHandle,
  RIS.fromHandle',
  MonadIO,
  liftIO,
  size,
  seek,
  tell,
  parse,
  inputStream,
  takeBytes,
  readExactly,
  dropExactly
)
where

import Data.Int
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import Control.Exception
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import Pdf.Toolbox.Core.Error
import qualified Pdf.Toolbox.Core.IO.RIS as RIS
import Pdf.Toolbox.Core.IO.RIS (RIS, IS)

-- | Total number of bytes in 'RIS'
size :: MonadIO m => RIS -> PdfE m Int64
size = tryPdfIO . RIS.size

-- | Change input position in 'RIS'
seek :: MonadIO m => RIS -> Int64 -> PdfE m ()
seek ris = tryPdfIO . RIS.seek ris

-- | Current input position
tell :: MonadIO m => RIS -> PdfE m Int64
tell = tryPdfIO . RIS.tell

-- | Parse from 'IS'
parse :: MonadIO m => Parser r -> IS -> PdfE m r
parse p is = do
  res <- liftIO $ (Right <$> Streams.parseFromStream p is)
    `catch` (\(Streams.ParseException str) -> return $ Left $ ParseError [] str)
    `catch` (\(e :: IOError) -> return $ Left $ IOError e)
  case res of
    Left e -> left e
    Right r -> return r

-- | Convert random access stream to sequential
inputStream :: MonadIO m => RIS -> PdfE m IS
inputStream = tryPdfIO . RIS.inputStream

-- | See 'Streams.takeBytes'
takeBytes :: MonadIO m => Int64 -> IS -> PdfE m IS
takeBytes n = tryPdfIO . Streams.takeBytes n

-- | See 'Streams.readExactly'
readExactly :: MonadIO m => Int -> IS -> PdfE m ByteString
readExactly n = tryPdfIO . Streams.readExactly n

-- | Same as 'readExactly', but ignores the result
dropExactly :: MonadIO m => Int -> IS -> PdfE m ()
dropExactly n is = readExactly n is >> return ()
