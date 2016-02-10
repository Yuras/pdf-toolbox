
-- | Mid level utils for processing PDF file
--
-- Basic example how to get number of pages in document
--
-- @
--  import Pdf.Document
--
--  withBinaryFile \"input.pdf\" ReadMode $ \handle ->
--    pdf <- 'pdfWithHandle' handle
--    doc <- 'document' pdf
--    catalog <- 'documentCatalog' doc
--    rootNode <- 'catalogPageNode' catalog
--    count <- 'pageNodeNKids' rootNode
--    print count
-- @

module Pdf.Document
(
  module Pdf.Document.Types,
  module Pdf.Document.Pdf,
  module Pdf.Document.Document,
  module Pdf.Document.Catalog,
  module Pdf.Document.PageNode,
  module Pdf.Document.Page,
  module Pdf.Document.Info,
  module Pdf.Document.FontDict,
  module Pdf.Core.Object.Types,
  module Pdf.Core.Object.Util
)
where

import Pdf.Core.Object.Types
import Pdf.Core.Object.Util

import Pdf.Document.Types
import Pdf.Document.Pdf
import Pdf.Document.Document
import Pdf.Document.Info
import Pdf.Document.Catalog
import Pdf.Document.PageNode
import Pdf.Document.Page
import Pdf.Document.FontDict
