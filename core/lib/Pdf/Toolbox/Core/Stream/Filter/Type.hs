{-# LANGUAGE DeriveDataTypeable #-}

-- | Stream filter

module Pdf.Toolbox.Core.Stream.Filter.Type
(
  StreamFilter(..),
  DecodeException(..)
)
where

import Data.Typeable
import Control.Exception

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.IO

-- | Stream filter
data StreamFilter = StreamFilter {
  filterName :: Name,      -- ^ as \"Filter\" key value in stream dictionary
  filterDecode :: Maybe Dict -> IS -> IO IS    -- ^ decode params -> content -> decoded content
}

-- | Exception that should be thrown by the decoder in case of any error
-- User code could catch it when reading from decoded stream content
data DecodeException = DecodeException (SomeException)
  deriving (Show, Typeable)

instance Exception DecodeException
