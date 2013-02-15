{-# LANGUAGE OverloadedStrings #-}

-- | PDF document

module Pdf.Toolbox.Document.Document
(
  Document,
  documentCatalog,
  documentEncryption
)
where

import Pdf.Toolbox.Core

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

-- | Document encryption dictionary
documentEncryption :: MonadPdf m => Document -> PdfE m (Maybe Dict)
documentEncryption (Document dict) = do
  case lookupDict' "Encrypt" dict of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref o >>= fromObject
      return $ Just o'
