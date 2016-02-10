
-- | Stream filter

module Pdf.Core.Stream.Filter.Type
(
  StreamFilter(..)
)
where

import Data.ByteString (ByteString)
import System.IO.Streams (InputStream)

import Pdf.Core.Object.Types
import Pdf.Core.Name (Name)

-- | Stream filter
data StreamFilter = StreamFilter {
  -- | as \"Filter\" key value in stream dictionary
  filterName :: Name,
  -- | decode params -> content -> decoded content
  filterDecode :: Maybe Dict
               -> InputStream ByteString
               -> IO (InputStream ByteString)
  }
