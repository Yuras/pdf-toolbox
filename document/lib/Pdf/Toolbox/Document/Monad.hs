
-- | Interface to the underlying PDF file

module Pdf.Toolbox.Document.Monad
(
  MonadPdf(..),
  deref
)
where

import Data.Int

import Pdf.Toolbox.Core

import Pdf.Toolbox.Document.Encryption

-- | Interface to the underlying PDF file
class Monad m => MonadPdf m where
  -- | find object by it's reference
  lookupObject :: Ref -> PdfE m (Object Int64)
  -- | decoded stream content
  --
  -- Note: the 'IS' returned is valid only until the next 'lookupObject'
  -- or any other operation, that requares seek
  streamContent :: Ref -> Stream Int64 -> PdfE m (Stream IS)
  -- | Current decryptor
  getDecryptor :: PdfE m (Maybe Decryptor)
  -- | Get random access input stream for direct access to the PDF file
  getRIS :: PdfE m RIS
  -- | Get all stream filters
  getStreamFilters :: PdfE m [StreamFilter]

-- | Recursively load indirect object
deref :: (MonadPdf m, Show a) => Object a -> PdfE m (Object ())
deref (ORef ref) = do
  o <- lookupObject ref
  deref o
deref o = return $ mapObject (const ()) o
