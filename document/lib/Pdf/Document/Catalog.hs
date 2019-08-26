{-# LANGUAGE OverloadedStrings #-}

-- | Document datalog

module Pdf.Document.Catalog
(
  Catalog,
  catalogPageNode
)
where

import Pdf.Core.Object.Util
import Pdf.Core.Exception
import Pdf.Core.Util

import Pdf.Document.Pdf
import Pdf.Document.Internal.Types
import Pdf.Document.Internal.Util

import qualified Data.HashMap.Strict as HashMap

-- | Get root node of page tree
catalogPageNode :: Catalog -> IO PageNode
catalogPageNode (Catalog pdf _ dict) = do
  ref <- sure $
    (HashMap.lookup "Pages" dict >>= refValue)
    `notice` "Pages should be an indirect reference"
  obj <- lookupObject pdf ref >>= deref pdf
  node <- sure $ dictValue obj `notice` "Pages should be a dictionary"
  ensureType "Pages" node
  return (PageNode pdf ref node)
