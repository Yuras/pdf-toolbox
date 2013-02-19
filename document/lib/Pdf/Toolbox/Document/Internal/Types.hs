
-- | Internal type declarations

module Pdf.Toolbox.Document.Internal.Types
(
  Document(..),
  Catalog(..),
  PageTree(..),
  PageNode(..),
  Page(..)
)
where

import Pdf.Toolbox.Core

-- | PDF document
--
-- It is a trailer under the hood
data Document = Document XRef Dict
  deriving Show

-- | Document catalog
data Catalog = Catalog Ref Dict
  deriving Show

-- | Page tree
data PageTree =
  PageTreeNode PageNode |
  PageTreeLeaf Page
  deriving Show

-- | Page tree node, contains pages or other nodes
data PageNode = PageNode Ref Dict
  deriving Show

-- | Pdf document page
data Page = Page Ref Dict
  deriving Show
