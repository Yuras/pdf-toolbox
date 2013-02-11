
-- | Low level tools for processing PDF file
--
-- Basic example how to access document catalog:
--
-- @
--  withBinaryFile \"input.pdf\" ReadMode $ \handle -> do
--    -- create random access input stream
--    ris <- 'fromHandle' handle
--    runEitherT $ do
--      -- find the last cross reference
--      xref <- 'lastXRef' ris
--      tr <- 'trailer' ris xref
--      -- \"Root\" element in trailer is an indirect object, pointing to document catalog
--      root \<- 'lookupDict' \"Root\" tr >>= 'fromObject'
--      -- retrieve the catralog itself
--      catalog \<- 'lookupObject' ris 'knownFilters' root >>= 'toStream'
--      liftIO $ print catalog
--      -- then use the catalog to access pages, outlines, resources, content streams, etc
-- @
--

module Pdf.Toolbox.Core
(
  module Pdf.Toolbox.Core.Error,
  module Pdf.Toolbox.Core.IO,
  module Pdf.Toolbox.Core.Stream,
  module Pdf.Toolbox.Core.Object.Types,
  module Pdf.Toolbox.Core.Object.Util,
  module Pdf.Toolbox.Core.XRef
)
where

import Pdf.Toolbox.Core.Error
import Pdf.Toolbox.Core.IO
import Pdf.Toolbox.Core.Stream
import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Util
import Pdf.Toolbox.Core.XRef
