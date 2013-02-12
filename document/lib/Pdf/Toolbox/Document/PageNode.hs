{-# LANGUAGE OverloadedStrings #-}

-- | Page tree node

module Pdf.Toolbox.Document.PageNode
(
  PageNode,
  PageTree(..),
  pageNodeNKids,
  pageNodeParent,
  pageNodeKids,
  loadPageNode,
  pageNodePageByNum
)
where

import Pdf.Toolbox.Core

import Pdf.Toolbox.Document.Monad
import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Document.Internal.Util

-- | Total number of child leaf nodes, including deep children
pageNodeNKids :: MonadPdf m => PageNode -> PdfE m Int
pageNodeNKids (PageNode _ dict) =
  lookupDict "Count" dict >>= fromObject >>= intValue

-- | Parent page node
pageNodeParent :: MonadPdf m => PageNode -> PdfE m (Maybe PageNode)
pageNodeParent (PageNode _ dict) =
  case lookupDict' "Parent" dict of
    Nothing -> return Nothing
    Just o -> do
      ref <- fromObject o
      node <- lookupObjectM ref >>= fromObject
      ensureType "Pages" node
      return $ Just $ PageNode ref node

-- | Referencies to all kids
pageNodeKids :: MonadPdf m => PageNode -> PdfE m [Ref]
pageNodeKids (PageNode _ dict) = do
  Array kids <- lookupDict "Kids" dict >>= fromObject
  mapM fromObject kids

-- | Load page tree node by reference
loadPageNode :: MonadPdf m => Ref -> PdfE m PageTree
loadPageNode ref = do
  node <- lookupObjectM ref >>= fromObject
  nodeType <- dictionaryType node
  case nodeType of
    "Pages" -> return $ PageTreeNode $ PageNode ref node
    "Page" -> return $ PageTreeLeaf $ Page ref node
    _ -> left $ UnexpectedError $ "Unexpected page tree node type: " ++ show nodeType

-- | Find page by it's number
--
-- Note: it is not efficient for PDF files with a lot of pages,
-- because it performs traversal through the page tree each time.
-- Use 'pageNodeNKids', 'pageNodeKids' and 'loadPageNode' for
-- efficient traversal.
pageNodePageByNum :: MonadPdf m => PageNode -> Int -> PdfE m Page
pageNodePageByNum node num = do
  pageNodeKids node >>= loop num
  where
  loop _ [] = left $ UnexpectedError "Page not found"
  loop i (x:xs) = do
    kid <- loadPageNode x
    case kid  of
      PageTreeNode n -> do
        nkids <- pageNodeNKids n
        if i < nkids
          then pageNodePageByNum n i
          else loop (i - nkids) xs
      PageTreeLeaf page ->
        if i == 0
          then return page
          else loop (i - 1) xs
