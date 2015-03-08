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
import Text.Printf

import Pdf.Toolbox.Core.Object.Types
import qualified Pdf.Toolbox.Core.Name as Name

-- | Build indirect object
buildIndirectObject :: Ref -> Object BSL.ByteString -> Builder
buildIndirectObject (Ref i g) object =
  char7 '\n' `mappend`
  intDec i `mappend`
  char7 ' ' `mappend`
  intDec g `mappend`
  byteString " obj\n" `mappend`
  build object `mappend`
  byteString "\nendobj\n"
  where
  build (OStream s) = buildStream s
  build o = buildObject o

-- | Render inline object (without \"obj/endobj\").
-- It is 'error' to supply 'Stream', because it could not
-- be inlined, but should always be an indirect object
buildObject :: Object a -> Builder
buildObject (ONumber n) = buildNumber n
buildObject (OBoolean b) = buildBool b
buildObject (OName n) = buildName n
buildObject (ODict d) = buildDict d
buildObject (OArray a) = buildArray a
buildObject (OStr s) = buildString s
buildObject (ORef r) = buildRef r
buildObject (OStream _) = error "buildObject: please don't pass streams to me"
buildObject ONull = byteString "null"

buildStream :: Stream BSL.ByteString -> Builder
buildStream (Stream dict content) =
  buildDict dict `mappend`
  byteString "stream\n" `mappend`
  lazyByteString content `mappend`
  byteString "\nendstream"

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

buildDict :: Dict -> Builder
buildDict (Dict xs) =
  byteString "<<" `mappend`
  intercalate (char7 ' ') (concatMap build xs) `mappend`
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

buildRef :: Ref -> Builder
buildRef (Ref i j) = intDec i `mappend` char7 ' ' `mappend` intDec j `mappend` byteString " R"
