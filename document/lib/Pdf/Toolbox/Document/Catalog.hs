{-# LANGUAGE OverloadedStrings #-}

-- | Document datalog

module Pdf.Toolbox.Document.Catalog
(
  Catalog,
  catalogPageNode
)
where

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Util

import Pdf.Toolbox.Document.Pdf
import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Document.Internal.Util

-- | Get root node of page tree
catalogPageNode :: Catalog -> IO PageNode
catalogPageNode (Catalog pdf _ dict) = do
  ref <- sure $
    (lookupDict "Pages" dict >>= refValue)
    `notice` "Pages should be an indirect reference"
  obj <- lookupObject pdf ref >>= deref pdf
  node <- sure $ dictValue obj `notice` "Pages should be a dictionary"
  ensureType "Pages" node
  return (PageNode pdf ref node)
