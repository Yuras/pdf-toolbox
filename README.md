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

Still in TODO list
------------------

 * Support for object streams (aka compressed objects)
 * Encrypted PDF documents (you can inspect document structure,
have to decrypt stream and string manually)
 * Incremental updates
 * Linearized PDF files

Examples
--------

Inspect low level structure:

	{-# LANGUAGE OverloadedStrings #-}

	import System.IO
	import Pdf.Toolbox.Core

	main = withBinaryFile "input.pdf" ReadMode $ \handle -> do
	    -- create random access input stream
	    ris <- fromHandle handle
	    runEitherT $ do
	      -- find the last cross reference
	      xref <- lastXRef ris
	      tr <- trailer ris xref
	      -- "Root" element in trailer is an indirect object, pointing to document catalog
	      root <- lookupDict "Root" tr >>= fromObject
	      -- retrieve the catralog itself
	      catalog <- lookupObject ris knownFilters root >>= toDict
	      liftIO $ print catalog
	      -- then use the catalog to access pages, outlines, resources, content streams, etc

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
