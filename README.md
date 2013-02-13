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
 * Encrypted PDF documents
 * Incremental updates
 * Linearized PDF files
