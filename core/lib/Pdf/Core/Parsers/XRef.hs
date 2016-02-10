{-# LANGUAGE OverloadedStrings #-}

-- | Parsers for XRef

module Pdf.Core.Parsers.XRef
(
  startXRef,
  tableXRef,
  parseSubsectionHeader,
  parseTrailerAfterTable,
  parseTableEntry
)
where

import Data.Int
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative (many)

import Pdf.Core.Object.Types
import Pdf.Core.Parsers.Object
import Pdf.Core.Parsers.Util

-- for doctest
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString.Char8

-- | Offset of the very last xref table
--
-- Before calling it, make sure your are currently somewhere near
-- the end of pdf file. Otherwice it can eat all the memory.
-- E.g. examine only the last 1KB
--
-- >>> parseOnly startXRef "anything...startxref\n222\n%%EOF...blah\nstartxref\n123\n%%EOF"
-- Right 123
startXRef :: Parser Int64
startXRef = do
  res <- many $ do
    _ <- P.manyTill P.anyChar $ P.string "startxref"
    P.skipSpace
    offset <- P.decimal
    P.skipSpace
    _ <- P.string "%%EOF"
    return offset
  case res of
    [] -> fail "Trailer not found"
    xs -> return $ last xs

-- | When current input position points to xref stream
-- (or doesn't point to xref at all), the parser will fail.
-- When it points to xref table, the parser will succeed
-- and input position will point to the first xref subsection
--
-- >>> parseOnly tableXRef "xref\n"
-- Right ()
-- >>> parseOnly tableXRef "not xref"
-- Left "Failed reading: takeWith"
tableXRef :: Parser ()
tableXRef = do
  _ <- P.string "xref"
  endOfLine

-- | Parse subsection header, return (the first object index, number of object)
--
-- Input position will point to the first object
parseSubsectionHeader :: Parser (Int, Int)
parseSubsectionHeader = do
  start <- P.decimal
  P.skipSpace
  count <- P.decimal
  endOfLine
  return (start, count)

-- | Parse trailer located after XRef table
--
-- Input position should point to the \"trailer\" keyword
parseTrailerAfterTable :: Parser Dict
parseTrailerAfterTable = do
  _ <- P.string "trailer"
  endOfLine
  P.skipSpace
  parseDict

-- | Parse XRef table entry. Returns offset, generation and whether the object is free.
parseTableEntry :: Parser (Int64, Int, Bool)
parseTableEntry = do
  offset <- P.decimal
  P.skipSpace
  generation <- P.decimal
  P.skipSpace
  c <- P.anyChar
  case c of
    'n' -> return (offset, generation, False)
    'f' -> return (offset, generation, True)
    _ -> fail $ "error parsing XRef table entry: unknown char: " ++ [c]
