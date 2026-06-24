{-# LANGUAGE OverloadedStrings #-}

-- | Interactive forms

module Pdf.Document.Form
(
  Form,
  formFields,
  Field,
  FieldType(..),
  fieldType,
  partialFieldName,
  alternativeFieldName,
  fieldKids,
  fieldFlags,
)
where

import Pdf.Core.Exception
import Pdf.Core.Util
import Pdf.Core.Object.Util

import Pdf.Document.Pdf
import Pdf.Document.Internal.Types
import Pdf.Document.Internal.Util

import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap

import Control.Monad

import Data.Text (Text)

-- | Form root fields
formFields :: Form -> IO [Field]
formFields (Form pdf dict) = do
  fields <- sure $
    (HashMap.lookup "Fields" dict >>= arrayValue >>= mapM refValue)
    `notice` "Fields should be an array of indirect references"
  fields' <- forM fields $ \ref -> do
    o <- lookupObject pdf ref
    dict' <- sure $ dictValue o
      `notice` "Field should be a dictionary"
    return (Field pdf ref dict')

  return (Vector.toList fields')

-- | The field type specified on the field
--
-- The field is only required for terminal fields
-- but may still be unset if it is inherited from a parent field
fieldType :: Field -> IO (Maybe FieldType)
fieldType (Field _ _ dict) =
  forM (HashMap.lookup "FT" dict) $ \ft -> do
    ft' <- sure $ nameValue ft
      `notice` "Field type should be a name"
    case ft' of
      "Btn" -> return FTButton
      "Tx"  -> return FTText
      "Ch"  -> return FTChoice
      "Sig" -> return FTSignature
      _     -> sure $ Left "Field type should be one of 'Btn', 'Tx', 'Ch' or 'Sig'"

-- | The partial field name
--
-- If missing, the field should be considered a widget annotation
-- (See [PDF Issue #28](https://github.com/pdf-association/pdf-issues/issues/28))
partialFieldName :: Field -> IO (Maybe Text)
partialFieldName (Field _ _ dict) =
  forM (HashMap.lookup "T" dict) $ \pn -> do
    pn' <- sure $ stringValue pn
      `notice` "Partial field name should be a string"
    decodeTextStringThrow pn'

-- | The alternative field name, which shall be used when the name is reported
-- to the user.
alternativeFieldName :: Field -> IO (Maybe Text)
alternativeFieldName (Field _ _ dict) =
  forM (HashMap.lookup "TU" dict) $ \an -> do
    an' <- sure $ stringValue an
      `notice` "Alternative field name should be a string"
    decodeTextStringThrow an'

-- | Field Kids
fieldKids :: Field -> IO (Maybe [Field])
fieldKids (Field pdf _ dict) =
  forM (HashMap.lookup "Kids" dict) $ \ks -> do
    kids <- sure $ (arrayValue ks >>= mapM refValue)
      `notice` "Kids should be an array of indirect references if it exists"
    kids' <- forM kids $ \ref -> do
      o <- lookupObject pdf ref
      dict' <- sure $ dictValue o
        `notice` "Field should be a dictionary"
      return (Field pdf ref dict')
    return (Vector.toList kids')

fieldFlags :: Field -> IO (Maybe Int)
fieldFlags (Field _ _ dict) =
  forM (HashMap.lookup "Ff" dict) $ \ff ->
    sure $ intValue ff
      `notice` "Ff (Field flags) should be an integer if it exists"

-- TODO:
-- fieldValue
