{-# LANGUAGE OverloadedStrings #-}

-- | PDF document page

module Pdf.Toolbox.Document.Page
(
  Page,
  pageParentNode
)
where

import Pdf.Toolbox.Core

import Pdf.Toolbox.Document.Monad
import Pdf.Toolbox.Document.PageNode
import Pdf.Toolbox.Document.Internal.Types

-- | Page's parent node
pageParentNode :: MonadPdf m => Page -> PdfE m PageNode
pageParentNode (Page _ dict) = do
  ref <- lookupDict "Parent" dict >>= fromObject
  node <- loadPageNode ref
  case node of
    PageTreeNode n -> return n
    PageTreeLeaf _ -> left $ UnexpectedError "page parent should be a note, but leaf should"
