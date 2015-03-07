
-- | Utils relayted to pdf objects

module Pdf.Toolbox.Core.Object.Util
(
  lookupDict,
  intValue,
  boolValue,
  realValue,
  nameValue,
  stringValue,
  arrayValue,
  streamValue,
  refValue,
  dictValue
)
where

import Data.ByteString (ByteString)

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

-- | Try to convert object to 'Bool'
boolValue :: Object a -> Maybe Bool
boolValue (OBoolean b) = Just b
boolValue _ = Nothing

-- | Try to convert object to 'Double'
--
-- Integral value automatically gets converted
realValue :: Object a -> Maybe Double
realValue (ONumber (NumReal d)) = Just d
realValue (ONumber (NumInt i)) = Just (fromIntegral i)
realValue _ = Nothing

-- | Try to convert object to 'Name'
nameValue :: Object a -> Maybe Name
nameValue (OName n) = Just n
nameValue _ = Nothing

-- | Try to convert object to 'ByteString'
stringValue :: Object a -> Maybe ByteString
stringValue (OStr (Str s)) = Just s
stringValue _ = Nothing

-- | Try to convert object to array
arrayValue :: Object a -> Maybe Array
arrayValue (OArray arr) = Just arr
arrayValue _ = Nothing

-- | Try to convert object to stream
streamValue :: Object a -> Maybe (Stream a)
streamValue (OStream s) = Just s
streamValue _ = Nothing

-- | Try to convert object to reference
refValue :: Object a -> Maybe Ref
refValue (ORef ref) = Just ref
refValue _ = Nothing

-- | Try to convert object to dictionary
dictValue :: Object a -> Maybe Dict
dictValue (ODict d) = Just d
dictValue _ = Nothing
