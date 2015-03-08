{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module contains definitions of pdf objects
--
-- See PDF1.7:7.3

module Pdf.Toolbox.Core.Object.Types
(
  Object(..),
  Dict,
  Array,
  Stream(..),
  Ref(..)
)
where

import Pdf.Toolbox.Core.Name (Name)

import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.Vector (Vector)
import Data.HashMap.Strict as HashMap

-- | Dictionary
type Dict = HashMap Name (Object ())

-- | An array
type Array = Vector (Object ())

-- | Contains stream dictionary and a payload
--
-- The payload could be offset within pdf file, actual content,
-- content stream or nothing
data Stream a = S Dict a
  deriving (Eq, Show)

-- | Object reference, contains object index and generation
data Ref = R Int Int
  deriving (Eq, Show, Ord)

-- | Any pdf object
--
-- It is parameterized by 'Stream' content
data Object a =
  Number Scientific |
  Boolean Bool |
  Name Name |
  Dict Dict |
  Array Array |
  String ByteString |
  Stream (Stream a) |
  Ref Ref |
  Null
  deriving (Eq, Show)
