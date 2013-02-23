{-# LANGUAGE OverloadedStrings #-}

-- | Render 'Object' to bytestring

module Pdf.Toolbox.Core.Object.Builder
(
  buildIndirectObject,
  buildObject,
  buildNumber,
  buildBoolean,
  buildName,
  buildDict,
  buildArray,
  buildStr,
  buildRef,
  buildStream
)
where

import Data.Monoid
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Text.Printf

import Pdf.Toolbox.Core.Object.Types

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
buildObject (OBoolean b) = buildBoolean b
buildObject (OName n) = buildName n
buildObject (ODict d) = buildDict d
buildObject (OArray a) = buildArray a
buildObject (OStr s) = buildStr s
buildObject (ORef r) = buildRef r
buildObject (OStream _) = error "buildObject: please don't pass streams to me"
buildObject ONull = byteString "null"

buildStream :: Stream BSL.ByteString -> Builder
buildStream (Stream dict content) =
  buildDict dict `mappend`
  byteString "stream\n" `mappend`
  lazyByteString content `mappend`
  byteString "\nendstream"

buildNumber :: Number -> Builder
buildNumber (NumInt i) = intDec i
buildNumber (NumReal d) = string7 $ printf "%f" d

buildBoolean :: Boolean -> Builder
buildBoolean (Boolean True) = byteString "true"
buildBoolean (Boolean False) = byteString "false"

buildName :: Name -> Builder
buildName (Name n) = char7 '/' `mappend` byteString n

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

buildArray :: Array -> Builder
buildArray (Array xs) =
  char7 '[' `mappend`
  intercalate (char7 ' ') (map buildObject xs) `mappend`
  char7 ']'

buildStr :: Str -> Builder
buildStr (Str s) =
  if BS8.all isPrint s
    then char7 '(' `mappend` (byteString . BS8.pack . concatMap escape . BS8.unpack $ s) `mappend` char7 ')'
    else char7 '<' `mappend` (byteString . BS.pack . concatMap toHex . BS.unpack $ s) `mappend` char7 '>'
  where
  toHex w = map (\a -> if a < 10 then a + 48 else a + 55) [w `div` 16, w `mod` 16]
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
