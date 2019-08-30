{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for internal use

module Pdf.Document.Internal.Util
(
  ensureType,
  dictionaryType,
  decodeTextString,
  decodeTextStringThrow
)
where

import Pdf.Core
import Pdf.Core.Exception
import qualified Pdf.Content.Encoding.PdfDoc as PdfDoc

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Exception hiding (throw)

-- | Check that the dictionary has the specified \"Type\" filed
ensureType :: Name -> Dict -> IO ()
ensureType name dict = do
  n <- sure $ dictionaryType dict
  unless (n == name) $
    throwIO $ Corrupted ("Expected type: " ++ show name ++
                       ", but found: " ++ show n) []

-- | Get dictionary type, name at key \"Type\"
dictionaryType :: Dict -> Either String Name
dictionaryType dict =
  case HashMap.lookup "Type" dict of
    Just (Name n) -> Right n
    Just _ -> Left "Type should be a name"
    _ -> Left "Type is missing"

decodeTextStringThrow :: ByteString -> IO Text
decodeTextStringThrow bs = case decodeTextString bs of
  Left err -> throwIO $ Corrupted err []
  Right txt -> return txt

decodeTextString :: ByteString -> Either String Text
decodeTextString bs
  | "\254\255" `ByteString.isPrefixOf` bs
  = Right (Text.decodeUtf16BEWith Text.ignore (ByteString.drop 2 bs))
  | otherwise
  = do
    chars <- forM (ByteString.unpack bs) $ \c ->
      maybe (Left "Unknow symbol") Right (Map.lookup c PdfDoc.encoding)
    return (Text.concat chars)
