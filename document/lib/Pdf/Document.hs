
-- | Mid level utils for processing PDF file
--
-- Basic example how to get number of pages in document
--
-- @
--  import Pdf.Document
--
--  withPdfFile \"input.pdf\" $ \\pdf ->
--    doc <- 'document' pdf
--    catalog <- 'documentCatalog' doc
--    rootNode <- 'catalogPageNode' catalog
--    count <- 'pageNodeNKids' rootNode
--    print count
--    page <- 'loadPageByNum' rootNode 1
--    text <- 'pageExtractText' page
--    print text
-- @

module Pdf.Document
( module Pdf.Document.Types
, module Pdf.Document.Pdf
, module Pdf.Document.Document
, module Pdf.Document.Catalog
, module Pdf.Document.PageNode
, module Pdf.Document.Page
, module Pdf.Document.Info
, module Pdf.Document.FontDict
)
where

import Pdf.Document.Types
import Pdf.Document.Pdf
import Pdf.Document.Document
import Pdf.Document.Info
import Pdf.Document.Catalog
import Pdf.Document.PageNode
import Pdf.Document.Page
import Pdf.Document.FontDict
