
-- | Utils relayted to pdf objects

module Pdf.Core.Object.Util
( intValue
, boolValue
, realValue
, nameValue
, stringValue
, arrayValue
, streamValue
, refValue
, dictValue
)
where

import Pdf.Core.Object

import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific

-- | Try to convert object to 'Int'
--
-- Floating value doesn't automatically get converted
intValue :: Object -> Maybe Int
intValue (Number n)
  = either (const Nothing) Just
  . floatingOrInteger
  $ n
intValue _ = Nothing

-- | Specialized to prevent defaulting warning
floatingOrInteger :: Scientific -> Either Double Int
floatingOrInteger = Scientific.floatingOrInteger

-- | Try to convert object to 'Bool'
boolValue :: Object -> Maybe Bool
boolValue (Bool b) = Just b
boolValue _ = Nothing

-- | Try to convert object to 'Double'
--
-- Integral value automatically gets converted
realValue :: Object -> Maybe Double
realValue (Number n)
  = either Just (Just . fromIntegral)
  . floatingOrInteger
  $ n
realValue _ = Nothing

-- | Try to convert object to 'Name'
nameValue :: Object -> Maybe Name
nameValue (Name n) = Just n
nameValue _ = Nothing

-- | Try to convert object to 'ByteString'
stringValue :: Object -> Maybe ByteString
stringValue (String s) = Just s
stringValue _ = Nothing

-- | Try to convert object to array
arrayValue :: Object -> Maybe Array
arrayValue (Array arr) = Just arr
arrayValue _ = Nothing

-- | Try to convert object to stream
streamValue :: Object -> Maybe Stream
streamValue (Stream s) = Just s
streamValue _ = Nothing

-- | Try to convert object to reference
refValue :: Object -> Maybe Ref
refValue (Ref ref) = Just ref
refValue _ = Nothing

-- | Try to convert object to dictionary
dictValue :: Object -> Maybe Dict
dictValue (Dict d) = Just d
dictValue _ = Nothing
