{-# LANGUAGE OverloadedStrings #-}

-- | Render 'Object' to bytestring

module Pdf.Toolbox.Core.Object.Builder
(
  buildIndirectObject,
  buildObject,
  buildNumber,
  buildBool,
  buildName,
  buildDict,
  buildArray,
  buildString,
  buildRef,
  buildStream
)
where

import Data.Monoid
import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Base16 as Base16
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Text.Printf

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Name (Name)
import qualified Pdf.Toolbox.Core.Name as Name

-- | Build indirect object
buildIndirectObject :: Ref -> Object BSL.ByteString -> Builder
buildIndirectObject (R i g) object =
  char7 '\n' `mappend`
  intDec i `mappend`
  char7 ' ' `mappend`
  intDec g `mappend`
  byteString " obj\n" `mappend`
  build object `mappend`
  byteString "\nendobj\n"
  where
  build (Stream s) = buildStream s
  build o = buildObject o

-- | Render inline object (without \"obj/endobj\").
-- It is 'error' to supply 'Stream', because it could not
-- be inlined, but should always be an indirect object
buildObject :: Object a -> Builder
buildObject (Number n) = buildNumber n
buildObject (Bool b) = buildBool b
buildObject (Name n) = buildName n
buildObject (Dict d) = buildDict d
buildObject (Array a) = buildArray a
buildObject (String s) = buildString s
buildObject (Ref r) = buildRef r
buildObject (Stream _) = error "buildObject: please don't pass streams to me"
buildObject Null = byteString "null"

-- | Build a stream
--
-- The function doesn't try to encode or encrypt the content
buildStream :: Stream BSL.ByteString -> Builder
buildStream (S dict content) = mconcat
  [ buildDict dict
  , byteString "stream\n"
  , lazyByteString content
  , byteString "\nendstream"
  ]

-- | Build a number
buildNumber :: Scientific -> Builder
buildNumber
  = either bFloat intDec
  . Scientific.floatingOrInteger
  where
  bFloat d = string7 $ printf "%f" (d :: Double)

-- | Build a bool
buildBool :: Bool -> Builder
buildBool True = byteString "true"
buildBool False = byteString "false"

-- | Build a name
buildName :: Name -> Builder
-- XXX: escaping
buildName n = char7 '/' `mappend` byteString (Name.toByteString n)

intercalate :: Builder -> [Builder] -> Builder
intercalate _ [] = mempty
intercalate sep (x:xs) = x `mappend` go xs
  where
  go [] = mempty
  go (y:ys) = sep `mappend` y `mappend` go ys

-- | Build a dictionary
buildDict :: Dict -> Builder
buildDict dict =
  byteString "<<" `mappend`
  intercalate (char7 ' ') (concatMap build $ HashMap.toList dict) `mappend`
  byteString ">>"
  where
  build (key, val) = [buildName key, buildObject val]

-- | Build an array
buildArray :: Array -> Builder
buildArray xs =
  char7 '[' `mappend`
  intercalate (char7 ' ') (map buildObject $ Vector.toList xs) `mappend`
  char7 ']'

-- | Build a string
--
-- It may produce literal or hex string based on the context.
buildString :: ByteString -> Builder
buildString s =
  if Char8.all isPrint s
    then mconcat
      [ char7 '('
      , byteString . Char8.pack . concatMap escape . Char8.unpack $ s
      , char7 ')'
      ]
    else mconcat
      [ char7 '<'
      , byteString $ Base16.encode s
      , char7 '>'
      ]
  where
  escape '(' = "\\("
  escape ')' = "\\)"
  escape '\\' = "\\\\"
  escape '\n' = "\\n"
  escape '\r' = "\\r"
  escape '\t' = "\\t"
  escape '\b' = "\\b"
  escape ch = [ch]

-- | Build a reference
buildRef :: Ref -> Builder
buildRef (R i j) = mconcat
  [ intDec i
  , char7 ' '
  , intDec j
  , byteString " R"
  ]
