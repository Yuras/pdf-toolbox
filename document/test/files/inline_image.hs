{-# LANGUAGE OverloadedStrings #-}

-- Generate PDF file with inline image

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
        (contentDict, contentData, contentRef)
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
          ])
        ]
      font = HashMap.fromList [
        ("Type", Name "Font"),
        ("Subtype", Name "Type1"),
        ("BaseFont", Name "Helvetica")
        ]
      contentDict = HashMap.fromList [
        ("Length", Number $ fromIntegral $ BSL.length contentData)
        ]
      contentData = "BT /F1 12 Tf 100 100 TD (Hello!!!) Tj ET\n"
        <> "q 100 0 0 100 0 0 cm\n"
        <> "BI /W 4 /H 4 /CS /RGB /BPC 8\n"
        <> "ID\n"
        <> "00000z0z00zzz00z0zzz0zzzEI aazazaazzzaazazzzazzz\n"
        <> "EI Q\n"
        <> "BT /F1 12 Tf 70 70 TD (World!!!) Tj ET\n"
      catalogRef = R 1 0
      rootNodeRef = R 2 0
      pageRef = R 3 0
      contentRef = R 4 0
      fontRef = R 5 0

  writer <- makeWriter Streams.stdout
  writeHeader writer
  forM_ objects $ \(obj, ref) ->
    writeObject writer ref obj
  forM_ streams $ \(dict, dat, ref) ->
    writeStream writer ref dict dat
  writeXRefTable writer 0 tr
