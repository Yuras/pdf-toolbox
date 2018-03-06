{-# OPTIONS_HADDOCK not-home #-}

-- | Internal type declarations

module Pdf.Document.Internal.Types
(
  Pdf(..),
  Document(..),
  Catalog(..),
  Info(..),
  PageNode(..),
  Page(..),
  PageTree(..),
  FontDict(..),
)
where

import Pdf.Core

import Pdf.Document.File
import Pdf.Document.Encryption (Decryptor)

import Data.HashMap.Strict as HashMap
import Data.IORef

type ObjectCache = (Bool, HashMap Ref Object)

data Pdf = Pdf File (IORef (Maybe Decryptor)) (IORef ObjectCache)

-- | PDF document
--
-- It is a trailer under the hood
data Document = Document Pdf Dict

-- | Document catalog
data Catalog = Catalog Pdf Ref Dict

-- | Information dictionary
data Info = Info Pdf Ref Dict

-- | Page tree node, contains pages or other nodes
data PageNode = PageNode Pdf Ref Dict

-- | Pdf document page
data Page = Page Pdf Ref Dict

-- | Page tree
data PageTree =
  PageTreeNode PageNode |
  PageTreeLeaf Page

-- | Font dictionary
data FontDict = FontDict Pdf Dict
