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

Still in TODO list
------------------

 * Improve support for object streams (aka compressed objects). Too slow right now.
 * Encrypted PDF documents (when the document you are processing doesn't uses object streams,
then you can inspect document structure, only have to decrypt streams and strings manually
if you need them)
 * Incremental updates
 * Linearized PDF files

Examples
--------

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
