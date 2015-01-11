
-- | Mid level utils for processing PDF file
--
-- Basic example how to get number of pages in document
--
-- @
--  import Pdf.Toolbox.Document
--
--  withBinaryFile \"input.pdf\" ReadMode $ \handle ->
--    pdf <- 'pdfWithHandle' handle
--    doc <- 'document' pdf
--    catalog <- 'documentCatalog' doc
--    rootNode <- 'catalogPageNode' catalog
--    count <- 'pageNodeNKids' rootNode
--    print count
-- @

module Pdf.Toolbox.Document
(
  module Pdf.Toolbox.Document.Types,
  module Pdf.Toolbox.Document.Pdf,
  module Pdf.Toolbox.Document.Document,
  module Pdf.Toolbox.Document.Catalog,
  module Pdf.Toolbox.Document.PageNode,
  module Pdf.Toolbox.Document.Page,
  module Pdf.Toolbox.Document.Info,
  module Pdf.Toolbox.Document.FontDict,
  module Pdf.Toolbox.Core.Object.Types,
  module Pdf.Toolbox.Core.Object.Util
)
where

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Util

import Pdf.Toolbox.Document.Types
import Pdf.Toolbox.Document.Pdf
import Pdf.Toolbox.Document.Document
import Pdf.Toolbox.Document.Info
import Pdf.Toolbox.Document.Catalog
import Pdf.Toolbox.Document.PageNode
import Pdf.Toolbox.Document.Page
import Pdf.Toolbox.Document.FontDict
