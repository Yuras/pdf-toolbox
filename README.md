pdf-toolbox
===========

[![Build Status](https://travis-ci.org/Yuras/pdf-toolbox.svg?branch=master)](https://travis-ci.org/Yuras/pdf-toolbox)

A collection of tools for processing PDF files

Stable and HEAD
---------------

See "stable" branch for Hackage version. The current "master" branch is in a middle of API
rewrite, see [here](http://blog.haskell-exists.com/yuras/posts/pdf-toolbox-release-future-api-changes-and-design-mistakes.html)
for details.


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
 * Text extraction with exact glyph positions (mostly works, but in progress yet).
It can be used e.g. to implement text selection and copying in pdf viewer
 * Full support of xref streams and object streams
 * Supports editing of PDF files (incremental updates)
 * Basic support for PDF file generating
 * Encrypted PDF documents are partially supported

Still in TODO list
------------------

 * Linearized PDF files
 * Content stream tools: extract text, images, etc (basic implementation is already included)
 * Higher level API for incremental updates and PDF generating

Examples
--------

(Also see `examples` and `viewer` directories)

Inspect high level structure:

```haskell
import System.IO
import Pdf.Document

main =
  withBinaryFile "input.pdf" ReadMode $ \handle ->
          pdf <- pdfWithHandle handle
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    count <- pageNodeNKids rootNode
    print count
    -- the first page of the document
    page <- pageNodePageByNum rootNode 0
    ...
```
