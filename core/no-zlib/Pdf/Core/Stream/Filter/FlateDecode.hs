
-- | Flate decode filter

module Pdf.Core.Stream.Filter.FlateDecode
(
  flateDecode
)
where

import Pdf.Core.Stream.Filter.Type

-- | Vary basic implementation. Only PNG-UP prediction is implemented
--
-- Nothing when zlib is disabled via cabal flag
flateDecode :: Maybe StreamFilter
flateDecode = Nothing
