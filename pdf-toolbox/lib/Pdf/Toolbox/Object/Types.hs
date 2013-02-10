
-- | Module contains definitions of pdf objects
--
-- See PDF1.7:7.3

module Pdf.Toolbox.Object.Types
(
  Object(..),
  Number(..),
  Boolean(..),
  Name(..),
  Dict(..),
  Array(..),
  Str(..),
  Stream(..),
  Ref(..)
)
where

import Data.String
import Data.ByteString (ByteString)

-- | Integer or real 
data Number =
  NumInt Int |
  NumReal Double
  deriving (Eq, Show)

-- | \"true\" or \"false\"
newtype Boolean = Boolean Bool
  deriving (Eq, Show)

-- | Names usually are used as keys in dictionaries
--
-- They starts with \'/\', but we strip it out, see 'Text.Pdf.Parser.Object.parseName'
newtype Name = Name ByteString
  deriving (Eq, Show)

-- | Set of key/value pairs
newtype Dict = Dict [(Name, Object ())]
  deriving (Eq, Show)

-- | An array
newtype Array = Array [Object ()]
  deriving (Eq, Show)

-- | Sequence of zero or more bytes
--
-- Represents both the literal and hexadecimal strings
newtype Str = Str ByteString
  deriving (Eq, Show)

-- | Contains stream dictionary and a payload
--
-- The payload could be offset within pdf file, actual content,
-- content stream or nothing
data Stream a = Stream Dict a
  deriving (Eq, Show)

-- | Object reference, contains object index and generation
data Ref = Ref Int Int
  deriving (Eq, Show)

-- | Any pdf object
--
-- It is parameterized by 'Stream' content
data Object a =
  ONumber Number |
  OBoolean Boolean |
  OName Name |
  ODict Dict |
  OArray Array |
  OStr Str |
  OStream (Stream a) |
  ORef Ref |
  ONull
  deriving (Eq, Show)

instance IsString Name where
  fromString = Name . fromString
