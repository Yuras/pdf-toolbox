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
  pageNodePageByNum,
)
where

import qualified Data.Vector as Vector
import Control.Monad
import Control.Exception

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Util

import Pdf.Toolbox.Document.Pdf
import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Document.Internal.Util

-- | Total number of child leaf nodes, including deep children
pageNodeNKids :: PageNode -> IO Int
pageNodeNKids (PageNode _ _ dict) = sure $
  (lookupDict "Count" dict >>= intValue)
  `notice` "Count should be an integer"

-- | Parent page node
pageNodeParent :: PageNode -> IO (Maybe PageNode)
pageNodeParent (PageNode pdf _ dict) =
  case lookupDict "Parent" dict of
    Nothing -> return Nothing
    Just o@(ORef ref) -> do
      obj <- deref pdf o
      node <- sure $ dictValue obj `notice` "Parent should be a dictionary"
      ensureType "Pages" node
      return $ Just (PageNode pdf ref node)
    _ -> throw (Corrupted "Parent should be an indirect ref" [])

-- | Referencies to all kids
pageNodeKids :: PageNode -> IO [Ref]
pageNodeKids (PageNode pdf _ dict) = do
  obj <- sure (lookupDict "Kids" dict
                `notice` "Page node should have Kids")
        >>= deref pdf
  kids <- sure $ arrayValue obj
    `notice` "Kids should be an array"
  forM (Vector.toList kids) $ \k -> sure $
    refValue k `notice` "each kid should be a reference"

-- | Load page tree node by reference
loadPageNode :: Pdf -> Ref -> IO PageTree
loadPageNode pdf ref = do
  obj <- lookupObject pdf ref >>= deref pdf
  node <- sure $ dictValue obj `notice` "page should be a dictionary"
  nodeType <- sure $ dictionaryType node
  case nodeType of
    "Pages" -> return $ PageTreeNode (PageNode pdf ref node)
    "Page" -> return $ PageTreeLeaf (Page pdf ref node)
    _ -> throw $ Corrupted ("Unexpected page tree node type: "
                              ++ show nodeType) []

-- | Find page by it's number
--
-- Note: it is not efficient for PDF files with a lot of pages,
-- because it performs traversal through the page tree each time.
-- Use 'pageNodeNKids', 'pageNodeKids' and 'loadPageNode' for
-- efficient traversal.
pageNodePageByNum :: PageNode -> Int -> IO Page
pageNodePageByNum node@(PageNode pdf nodeRef _) num =
  message ("page #" ++ show num ++ " for node: " ++ show nodeRef) $ do
  pageNodeKids node >>= loop num
  where
  loop _ [] = throw $ Corrupted "Page not found" []
  loop i (x:xs) = do
    kid <- loadPageNode pdf x
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
