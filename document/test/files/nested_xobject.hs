{-# LANGUAGE OverloadedStrings #-}

-- Generate PDF file with nested XObjects
-- See https://github.com/Yuras/pdf-toolbox/issues/48

module Main
(
  main
)
where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import qualified System.IO.Streams as Streams

import Pdf.Core
import Pdf.Core.Writer

main :: IO ()
main = do
  let tr = HashMap.fromList [
        ("Size", Number $ fromIntegral $ length objects),
        ("Root", Ref catalogRef)
        ]
      objects = [
        (Dict catalog, catalogRef),
        (Dict rootNode, rootNodeRef),
        (Dict page, pageRef),
        (Dict font, fontRef)
        ]
      streams = [
        (contentDict, contentData, contentRef),
        (xobjDict, xobjData, xobjRef),
        (xobj1Dict, xobj1Data, xobj1Ref)
        ]
      catalog = HashMap.fromList [
        ("Type", Name "Catalog"),
        ("Pages", Ref rootNodeRef)
        ]
      rootNode = HashMap.fromList [
        ("Type", Name "Pages"),
        ("Kids", Array $ Vector.fromList [Ref pageRef]),
        ("Count", Number 1)
        ]
      page = HashMap.fromList [
        ("Type", Name "Page"),
        ("Parent", Ref rootNodeRef),
        ("Contents", Ref contentRef),
        ("Resources", Dict resourcesDict),
        ("MediaBox", Array $ Vector.fromList [
          Number 0,
          Number 0,
          Number 200,
          Number 200
          ])
        ]
      resourcesDict = HashMap.fromList [
        ("Font", Dict $ HashMap.fromList [
          ("F1", Ref fontRef)
          ]),
        ("XObject", Dict $ HashMap.fromList [
          ("X1", Ref xobjRef)
          ])
        ]
      font = HashMap.fromList [
        ("Type", Name "Font"),
        ("Subtype", Name "Type1"),
        ("BaseFont", Name "Helvetica")
        ]
      xobjResourceDict = HashMap.fromList [
        ("Font", Dict $ HashMap.fromList [
          ("F1", Ref fontRef)
          ]),
        ("XObject", Dict $ HashMap.fromList [
          ("X1", Ref xobj1Ref)
          ])
        ]
      xobjDict = HashMap.fromList [
        ("Length", Number $ fromIntegral $ BSL.length xobjData),
        ("Type", Name "XObject"),
        ("Subtype", Name "Form"),
        ("Resources", Dict xobjResourceDict),
        ("BBox", Array $ Vector.fromList [
          Number 0,
          Number 0,
          Number 200,
          Number 200
          ])
        ]
      xobjData = "BT /F1 12 Tf 10 10 TD (XObject is here) Tj ET /X1 Do"
      xobj1ResourceDict = HashMap.fromList [
        ("Font", Dict $ HashMap.fromList [
          ("F1", Ref fontRef)
          ])
        ]
      xobj1Dict = HashMap.fromList [
        ("Length", Number $ fromIntegral $ BSL.length xobj1Data),
        ("Type", Name "XObject"),
        ("Subtype", Name "Form"),
        ("Resources", Dict xobj1ResourceDict),
        ("BBox", Array $ Vector.fromList [
          Number 0,
          Number 0,
          Number 200,
          Number 200
          ])
        ]
      xobj1Data = "BT /F1 12 Tf 50 50 TD (nested XObject is here) Tj ET"
      contentDict = HashMap.fromList [
        ("Length", Number $ fromIntegral $ BSL.length contentData)
        ]
      contentData = "BT /F1 12 Tf 100 100 TD (Hello World!!!) Tj ET /X1 Do"
      catalogRef = R 1 0
      rootNodeRef = R 2 0
      pageRef = R 3 0
      contentRef = R 4 0
      fontRef = R 5 0
      xobjRef = R 6 0
      xobj1Ref = R 7 0

  writer <- makeWriter Streams.stdout
  writeHeader writer
  forM_ objects $ \(obj, ref) ->
    writeObject writer ref obj
  forM_ streams $ \(dict, dat, ref) ->
    writeStream writer ref dict dat
  writeXRefTable writer 0 tr
