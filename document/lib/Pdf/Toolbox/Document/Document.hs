{-# LANGUAGE OverloadedStrings #-}

-- | PDF document

module Pdf.Toolbox.Document.Document
(
  Document,
  documentCatalog
)
where

import Pdf.Toolbox.Core hiding (lookupObject)

import Pdf.Toolbox.Document.Monad
import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Document.Internal.Util

-- | Get the document catalog
documentCatalog :: MonadPdf m => Document -> PdfE m Catalog
documentCatalog (Document dict) = do
  ref <- lookupDict "Root" dict >>= fromObject
  cat <- lookupObject ref >>= fromObject
  ensureType "Catalog" cat
  return $ Catalog ref cat
