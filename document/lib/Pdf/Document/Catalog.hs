{-# LANGUAGE OverloadedStrings #-}

-- | Document datalog

module Pdf.Document.Catalog
(
  Catalog,
  catalogPageNode,
  catalogForm,
)
where

import Pdf.Core.Object.Util
import Pdf.Core.Exception
import Pdf.Core.Util

import Pdf.Document.Pdf
import Pdf.Document.Internal.Types
import Pdf.Document.Internal.Util

import qualified Data.HashMap.Strict as HashMap

import Control.Monad

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

catalogForm :: Catalog -> IO (Maybe Form)
catalogForm (Catalog pdf _ dict) =
  forM (HashMap.lookup "AcroForm" dict) $ \o -> do
    o' <- deref pdf o
    node <- sure $ dictValue o'
      `notice` "AcroForm should be an indirect reference if it exists"
    return (Form pdf node)
