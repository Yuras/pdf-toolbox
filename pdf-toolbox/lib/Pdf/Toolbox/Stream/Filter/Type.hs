
-- | Stream filter

module Pdf.Toolbox.Stream.Filter.Type
(
  StreamFilter(..)
)
where

import Pdf.Toolbox.Object.Types
import Pdf.Toolbox.IO.RIS

-- | Stream filter
data StreamFilter = StreamFilter {
  filterName :: Name,      -- ^ as \"Filter\" key value in stream dictionary
  filterDecode :: Maybe Dict -> IS -> IO IS    -- ^ decode params -> content -> decoded content
}
