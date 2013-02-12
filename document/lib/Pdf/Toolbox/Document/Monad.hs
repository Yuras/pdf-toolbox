
-- | Interface to the underlying PDF file

module Pdf.Toolbox.Document.Monad
(
  MonadPdf(..)
)
where

import Data.Int

import Pdf.Toolbox.Core

-- | Interface to the underlying PDF file
class Monad m => MonadPdf m where
  -- | find object by it's reference
  lookupObject :: Ref -> PdfE m (Object Int64)
  getRIS :: PdfE m RIS
