pdf-toolbox
===========

[![Haskell CI](https://github.com/Yuras/pdf-toolbox/actions/workflows/build.yml/badge.svg)](https://github.com/Yuras/pdf-toolbox/actions/workflows/build.yml)

A collection of tools for processing PDF files


Features
--------

 * Written in Haskell
 * Parsing on demand. You don't need to parse or load into memory
the entire PDF file just to extract one image
 * Different levels of abstraction. You can inspect high level (catalog, page tree, pages)
or low level (xref, trailer, object) structure of PDF file.
You can even switch between levels of details on the fly.
 * Extremely fast and memory efficient when you need to inspect only part of the document
 * Resonably fast and memory efficient in general case
 * Text extraction with exact glyph positions
It can be used e.g. to implement text selection and copying in pdf viewer
 * Full support of xref streams and object streams
 * Supports editing of PDF files (incremental updates)
 * Basic support for PDF file generating
 * Encrypted PDF documents are partially supported

Still in TODO list
------------------

 * Linearized PDF files
 * Higher level API for incremental updates and PDF generating

Examples
--------

(Also see `examples` and `viewer` directories)

Inspect high level structure:

```haskell
import Pdf.Document

main =
  withPdfFile "input.pdf" $ \pdf ->
    encrypted <- isEncrypted pdf
    when encrypted $ do
      ok <- setUserPassword pdf defaultUserPassword
      unless ok $
        fail "need password"
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    count <- pageNodeNKids rootNode
    print count
    -- the first page of the document
    page <- pageNodePageByNum rootNode 0
    -- extract text
    txt <- pageExtractText page
    print txt
    ...
```
