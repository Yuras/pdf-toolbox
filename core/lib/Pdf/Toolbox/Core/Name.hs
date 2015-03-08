{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Name is an atomic symbol
--
-- Note that `IsString` instance is a partial function

module Pdf.Toolbox.Core.Name
(
  Name,
  make,
  toByteString
)
where

import Data.Monoid
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Hashable (Hashable)

-- | Names usually are used as keys in dictionaries
--
-- Byte 0 is not allowed inside names
newtype Name = Name ByteString
  deriving (Eq, Show, Ord, Monoid, Hashable)

-- | Make a name.
--
-- Throws if the bytestring contains 0
make :: ByteString -> Either String Name
make bs
  | ByteString.any (== 0) bs
  = Left "Name.make: 0 byte is not allowed"
  | otherwise
  = Right (Name bs)

-- | Unwrap name to bytestring
toByteString :: Name -> ByteString
toByteString (Name bs) = bs

instance IsString Name where
  fromString = either error id . make . fromString
