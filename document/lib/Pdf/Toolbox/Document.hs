
-- | Mid level utils for processing PDF file
--
-- Basic example how to get number of pages in document
--
-- @
--  withBinaryFile \"input.pdf\" ReadMode $ \handle ->
--    'runPdfWithHandle' handle 'knownFilters' $ do
--      pdf <- 'document'
--      catalog <- 'documentCatalog' pdf
--      rootNode <- 'catalogPageNode' catalog
--      cout <- 'pageNodeNKids' rootNode
--      liftIO $ print count
-- @

module Pdf.Toolbox.Document
(
  module Pdf.Toolbox.Document.Types,
  module Pdf.Toolbox.Document.Monad,
  module Pdf.Toolbox.Document.Pdf,
  module Pdf.Toolbox.Document.Document,
  module Pdf.Toolbox.Document.Catalog,
  module Pdf.Toolbox.Document.PageNode,
  module Pdf.Toolbox.Document.Page,
  module Pdf.Toolbox.Core.Error,
  module Pdf.Toolbox.Core.Object.Types,
  module Pdf.Toolbox.Core.Object.Util,
  knownFilters,
  MonadIO(..)
)
where

import Pdf.Toolbox.Core (knownFilters, MonadIO(..))
import Pdf.Toolbox.Core.Error
import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Util

import Pdf.Toolbox.Document.Types
import Pdf.Toolbox.Document.Monad
import Pdf.Toolbox.Document.Pdf
import Pdf.Toolbox.Document.Document
import Pdf.Toolbox.Document.Catalog
import Pdf.Toolbox.Document.PageNode
import Pdf.Toolbox.Document.Page
