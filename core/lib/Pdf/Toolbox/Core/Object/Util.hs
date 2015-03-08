
-- | Utils relayted to pdf objects

module Pdf.Toolbox.Core.Object.Util
(
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
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific

import Pdf.Toolbox.Core.Object.Types

-- | Try to convert object to 'Int'
--
-- Floating value doesn't automatically get converted
intValue :: Object a -> Maybe Int
intValue (ONumber n)
  = either (const Nothing) Just
  . floatingOrInteger
  $ n
intValue _ = Nothing

floatingOrInteger :: Scientific -> Either Double Int
floatingOrInteger = Scientific.floatingOrInteger

-- | Try to convert object to 'Bool'
boolValue :: Object a -> Maybe Bool
boolValue (OBoolean b) = Just b
boolValue _ = Nothing

-- | Try to convert object to 'Double'
--
-- Integral value automatically gets converted
realValue :: Object a -> Maybe Double
realValue (ONumber n)
  = either Just (Just . fromIntegral)
  . floatingOrInteger
  $ n
realValue _ = Nothing

-- | Try to convert object to 'Name'
nameValue :: Object a -> Maybe Name
nameValue (OName n) = Just n
nameValue _ = Nothing

-- | Try to convert object to 'ByteString'
stringValue :: Object a -> Maybe ByteString
stringValue (OStr s) = Just s
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
