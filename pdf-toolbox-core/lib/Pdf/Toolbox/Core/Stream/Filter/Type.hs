
-- | Stream filter

module Pdf.Toolbox.Core.Stream.Filter.Type
(
  StreamFilter(..)
)
where

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.IO

-- | Stream filter
data StreamFilter = StreamFilter {
  filterName :: Name,      -- ^ as \"Filter\" key value in stream dictionary
  filterDecode :: Maybe Dict -> IS -> IO IS    -- ^ decode params -> content -> decoded content
}
