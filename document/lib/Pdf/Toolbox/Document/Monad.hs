
-- | Interface to the underlying PDF file

module Pdf.Toolbox.Document.Monad
(
  MonadPdf(..),
  deref
)
where

import Data.Int

import Pdf.Toolbox.Core hiding (lookupObject)

-- | Interface to the underlying PDF file
class Monad m => MonadPdf m where
  -- | find object by it's reference
  lookupObject :: Ref -> PdfE m (Object Int64)
  -- | decoded stream content
  streamContent :: Stream Int64 -> PdfE m (Stream IS)
  -- | underlying 'RIS'
  getRIS :: PdfE m RIS

-- | Recursively load indirect object
deref :: (MonadPdf m, Show a) => Object a -> PdfE m (Object ())
deref (ORef ref) = do
  o <- lookupObject ref
  deref o
deref o = return $ mapObject (const ()) o
