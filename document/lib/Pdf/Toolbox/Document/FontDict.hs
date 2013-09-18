{-# LANGUAGE OverloadedStrings #-}

-- | Font dictionary

module Pdf.Toolbox.Document.FontDict
(
  FontDict,
  FontSubtype(..),
  fontDictSubtype
)
where

import Pdf.Toolbox.Core

import Pdf.Toolbox.Document.Internal.Types

-- | Font subtypes
data FontSubtype
  = FontType0
  | FontType1
  | FontMMType1
  | FontType3
  | FontTrueType
  deriving (Show, Eq)

-- | Get font subtype
fontDictSubtype :: Monad m => FontDict -> PdfE m FontSubtype
fontDictSubtype (FontDict dict) = do
  Name str <- lookupDict "Subtype" dict >>= fromObject
  case str of
    "Type0" -> return FontType0
    "Type1" -> return FontType1
    "MMType1" -> return FontMMType1
    "Type3" -> return FontType3
    "TrueType" -> return FontTrueType
    _ -> left $ UnexpectedError $ "Unexpected font subtype: " ++ show str
