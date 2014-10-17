
-- | Utils relayted to pdf objects

module Pdf.Toolbox.Core.Object.Util
(
  lookupDict,
  intValue
)
where

import Pdf.Toolbox.Core.Object.Types

-- | Lookup object by key
lookupDict :: Name -> Dict -> Maybe (Object ())
lookupDict key (Dict d) = lookup key d

-- | Try to convert object to 'Int'
--
-- Floating value doesn't automatically get converted
intValue :: Object a -> Maybe Int
intValue (ONumber (NumInt i)) = Just i
intValue _ = Nothing
