{-# LANGUAGE OverloadedStrings #-}

-- | Hello World in PDF :)
--
-- Right now there are no high level tools for PDF generating.
-- This example is very low level. Nothing more then proof
-- of concept.

module Main
(
  main
)
where

import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Builder
import System.IO

import Pdf.Toolbox.Core

main :: IO ()
main = do
  let patch = buildPatch objects (fromIntegral $ BS.length header) tr
      header = "%PDF-1.7\n"
      tr = Dict [
        ("Size", ONumber $ NumInt $ length objects),
        ("Root", ORef catalogRef)
        ]
      objects = [
        (Just $ ODict catalog, catalogRef),
        (Just $ ODict rootNode, rootNodeRef),
        (Just $ ODict page, pageRef),
        (Just $ OStream content, contentRef),
        (Just $ ODict font, fontRef)
        ]
      catalog = Dict [
        ("Type", OName "Catalog"),
        ("Pages", ORef rootNodeRef)
        ]
      rootNode = Dict [
        ("Type", OName "Pages"),
        ("Kids", OArray $ Array [ORef pageRef]),
        ("Count", ONumber $ NumInt 1)
        ]
      page = Dict [
        ("Type", OName "Page"),
        ("Parent", ORef rootNodeRef),
        ("Contents", ORef contentRef),
        ("Resources", ODict resourcesDict),
        ("MediaBox", OArray $ Array [
          ONumber (NumInt 0),
          ONumber (NumInt 0),
          ONumber (NumInt 200),
          ONumber (NumInt 200)
          ])
        ]
      resourcesDict = Dict [
        ("Font", ODict $ Dict [
          ("F1", ORef fontRef)
          ])
        ]
      font = Dict [
        ("Type", OName "Font"),
        ("Subtype", OName "Type1"),
        ("BaseFont", OName "Helvetica")
        ]
      content = Stream contentDict contentData
      contentDict = Dict [
        ("Length", ONumber $ NumInt $ fromIntegral $ BSL.length contentData)
        ]
      contentData = "BT /F1 12 Tf 100 100 TD (Hello World!!!) Tj ET"
      catalogRef = Ref 1 0
      rootNodeRef = Ref 2 0
      pageRef = Ref 3 0
      contentRef = Ref 4 0
      fontRef = Ref 5 0
  hPutBuilder stdout $ byteString header `mappend` patch
