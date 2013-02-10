
-- | IO Monad restrected to reading data from PDF file

module Pdf.Toolbox.IO.Monad
(
  MonadPdfIO(..),
  parseRIS
)
where

import Data.Int
import Data.Attoparsec.ByteString (Parser)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec (parseFromStream)

import Pdf.Toolbox.Error
import Pdf.Toolbox.IO.RIS (RIS, IS)
import qualified Pdf.Toolbox.IO.RIS as RIS

-- | IO Monad restrected to reading data from PDF file
class (Functor m, Monad m) => MonadPdfIO m where
  seek :: RIS -> Int64 -> PdfE m ()
  tell :: RIS -> PdfE m Int64
  size :: RIS -> PdfE m Int64
  parse :: IS -> Parser r -> PdfE m r
  inputStream :: RIS -> PdfE m IS
  -- | See 'Streams.takeBytes'
  takeBytes :: Int64 -> IS -> PdfE m IS

-- | Parse from 'RIS'
parseRIS :: MonadPdfIO m => RIS -> Parser r -> PdfE m r
parseRIS ris parser = do
  is <- inputStream ris
  parse is parser

instance MonadPdfIO IO where
  seek ris = tryIO . RIS.seek ris
  tell = tryIO . RIS.position
  size = tryIO . RIS.size
  inputStream = tryIO . RIS.inputStream
  parse is p = tryIO $ parseFromStream p is
  takeBytes n = tryIO . Streams.takeBytes n
