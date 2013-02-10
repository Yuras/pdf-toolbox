{-# LANGUAGE ScopedTypeVariables #-}

-- | IO Monad restrected to reading data from PDF file

module Pdf.Toolbox.IO.Monad
(
  MonadPdfIO(..),
  parseRIS
)
where

import Data.Int
import Data.Functor
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString (Parser)
import Control.Exception (catch)
import Control.Monad.IO.Class
import Control.Error hiding (tryIO)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import Pdf.Toolbox.Error
import Pdf.Toolbox.Object.Types
import Pdf.Toolbox.Stream.Filter.Type
import Pdf.Toolbox.IO.RIS (RIS, IS)
import qualified Pdf.Toolbox.IO.RIS as RIS

-- | IO Monad restrected to reading data from PDF file
class (Functor m, Monad m) => MonadPdfIO m where
  seek :: RIS -> Int64 -> PdfE m ()
  tell :: RIS -> PdfE m Int64
  size :: RIS -> PdfE m Int64
  parse :: IS -> Parser r -> PdfE m r
  inputStream :: RIS -> PdfE m IS
  trace :: Show a => a -> PdfE m ()
  -- | See 'Streams.takeBytes'
  takeBytes :: Int64 -> IS -> PdfE m IS
  -- | See 'Streams.readExactly'
  readExactly :: Int -> IS -> PdfE m ByteString
  dropExactly :: Int -> IS -> PdfE m ()
  applyFilter :: StreamFilter -> Maybe Dict -> IS -> PdfE m IS

-- | Parse from 'RIS'
parseRIS :: MonadPdfIO m => RIS -> Parser r -> PdfE m r
parseRIS ris parser = do
  is <- inputStream ris
  parse is parser

instance MonadPdfIO IO where
  seek ris = tryIO . RIS.seek ris
  tell = tryIO . RIS.position
  size = tryIO . RIS.size
  parse is p = do
    res <- liftIO $ (Right <$> Streams.parseFromStream p is)
      `catch` (\(Streams.ParseException str) -> return $ Left $ ParseError [] str)
      `catch` (\(e :: IOError) -> return $ Left $ IOError e)
    case res of
      Left e -> left e
      Right r -> return r
  inputStream = tryIO . RIS.inputStream
  trace a = liftIO $ print a
  takeBytes n = tryIO . Streams.takeBytes n
  readExactly n = tryIO . Streams.readExactly n
  dropExactly n ris = tryIO $ Streams.readExactly n ris >> return ()
  applyFilter f params = tryIO . filterDecode f params
