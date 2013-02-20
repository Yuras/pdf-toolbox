pdf-toolbox
===========

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

	import System.IO
	import Pdf.Toolbox.Document

	main =
	  withBinaryFile "input.pdf" ReadMode $ \handle ->
	    runPdfWithHandle handle knownFilters $ do
	      pdf <- document
	      catalog <- documentCatalog pdf
	      rootNode <- catalogPageNode catalog
	      count <- pageNodeNKids rootNode
	      liftIO $ print count
	      -- the first page of the document
	      page <- pageNodePageByNum rootNode 0
	      liftIO $ print page

Install
-------

The library uses `io-streams` package. It is not in hackage yet.
Clone it from the [repo](https://github.com/snapframework/io-streams)

Then use `cabal install` as usual
