{-# LANGUAGE OverloadedStrings #-}

-- | PDF document page

module Pdf.Toolbox.Document.Page
(
  Page,
  pageParentNode,
  pageContents
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

-- | List of references to page's content streams
pageContents :: MonadPdf m => Page -> PdfE m [Ref]
pageContents (Page _ dict) = do
  case lookupDict' "Contents" dict of
    Nothing -> return []
    Just (ORef ref) -> return [ref]
    Just (OArray (Array objs)) -> mapM fromObject objs
    _ -> left $ UnexpectedError "Unexpected value in page contents"
