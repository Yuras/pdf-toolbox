
-- | Stream filter

module Pdf.Toolbox.Core.Stream.Filter.Type
(
  StreamFilter(..)
)
where

import Data.ByteString (ByteString)
import System.IO.Streams (InputStream)

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Name (Name)

-- | Stream filter
data StreamFilter = StreamFilter {
  -- | as \"Filter\" key value in stream dictionary
  filterName :: Name,
  -- | decode params -> content -> decoded content
  filterDecode :: Maybe Dict
               -> InputStream ByteString
               -> IO (InputStream ByteString)
  }
