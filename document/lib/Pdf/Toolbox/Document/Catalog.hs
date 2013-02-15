{-# LANGUAGE OverloadedStrings #-}

-- | Document datalog

module Pdf.Toolbox.Document.Catalog
(
  Catalog,
  catalogPageNode
)
where

import Pdf.Toolbox.Core

import Pdf.Toolbox.Document.Monad
import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Document.Internal.Util

-- | Get root node of page tree
catalogPageNode :: MonadPdf m => Catalog -> PdfE m PageNode
catalogPageNode (Catalog _ dict) = do
  ref <- lookupDict "Pages" dict >>= fromObject
  node <- lookupObject ref >>= fromObject
  ensureType "Pages" node
  return $ PageNode ref node
