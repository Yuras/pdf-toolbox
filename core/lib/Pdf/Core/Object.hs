{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module contains definitions of pdf objects
--
-- See PDF1.7:7.3

module Pdf.Core.Object
( Object(..)
, Dict
, Array
, Stream(..)
, Ref(..)
, Name
)
where

import Pdf.Core.Name (Name)

import Data.Int
import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.Vector (Vector)
import Data.HashMap.Strict as HashMap

-- | Dictionary
type Dict = HashMap Name Object

-- | An array
type Array = Vector Object

-- | Contains stream dictionary and an offset in file
data Stream = S Dict Int64
  deriving (Eq, Show)

-- | Object reference, contains object index and generation
data Ref = R Int Int
  deriving (Eq, Show, Ord)

-- | Any pdf object
data Object =
  Number Scientific |
  Bool Bool |
  Name Name |
  Dict Dict |
  Array Array |
  String ByteString |
  Stream Stream |
  Ref Ref |
  Null
  deriving (Eq, Show)
