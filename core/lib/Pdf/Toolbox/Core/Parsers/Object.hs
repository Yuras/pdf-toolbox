{-# LANGUAGE OverloadedStrings #-}

-- | This module contains parsers for pdf objects

module Pdf.Toolbox.Core.Parsers.Object
(
  -- * Parse any object
  parseObject,
  -- * Parse object of specific type
  parseDict,
  parseArray,
  parseName,
  parseString,
  parseHexString,
  parseRef,
  parseNumber,
  parseBool,
  -- * Other
  parseTillStreamData,
  parseIndirectObject,
  isRegularChar
)
where

import Data.Char
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Applicative
import Control.Monad

import qualified Pdf.Toolbox.Core.Name as Name
import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Parsers.Util

-- | Parse a dictionary
parseDict :: Parser Dict
parseDict = do
  void $ P.string "<<"
  dict <- many parseKey
  P.skipSpace
  void $ P.string ">>"
  return $ HashMap.fromList dict

parseKey :: Parser (Name, Object ())
parseKey = do
  P.skipSpace
  key <- parseName
  val <- parseObject
  return (key, val)

-- | Parse an array
parseArray :: Parser Array
parseArray = do
  void $ P.char '['
  array <- many parseObject
  P.skipSpace
  void $ P.char ']'
  return $ Vector.fromList array

-- | Parse number
parseNumber :: Parser Scientific
parseNumber = P.choice [
  P.scientific,
  Scientific.fromFloatDigits <$>
    (P.signed
      $ read
      . ("0."++)
      . BS8.unpack <$>
        (P.char '.' >> P.takeWhile1 isDigit) :: Parser Double)
  ]

-- | Parse literal string
parseString :: Parser ByteString
parseString = do
  void $ P.char '('
  str <- takeStr 0 []
  return $ BS8.pack str
  where
  takeStr :: Int -> String -> Parser String
  takeStr lvl res = do
    ch <- P.anyChar
    case ch of
      '(' -> takeStr (lvl + 1) (ch : res)
      ')' -> if lvl == 0
               then return $ reverse res
               else takeStr (lvl - 1) (ch : res)
      '\\' -> do
        ch' <- P.anyChar
        if ch' `elem` "()\\"
          then takeStr lvl (ch' : res)
          else case ch' of
                 'r' -> takeStr lvl ('\r' : res)
                 'n' -> takeStr lvl ('\n' : res)
                 'f' -> takeStr lvl ('\f' : res)
                 'b' -> takeStr lvl ('\b' : res)
                 't' -> takeStr lvl ('\t' : res)
                 '\r' -> takeStr lvl res
                 _ -> do
                   ds <- take3Digits [ch']
                   let i = toEnum
                         . foldl'
                             (\acc (a, b) -> acc + a * charToInt b)
                             0
                         . zip [1, 8, 64]
                         $ ds
                   takeStr lvl (i : res)
      _ -> takeStr lvl (ch : res)
  charToInt ch = fromEnum ch - 48
  take3Digits ds
    | length ds >= 3
    = return ds
    | otherwise
    = do
      d <- P.peekChar'
      if isDigit d
        then do
          void P.anyChar
          take3Digits (d : ds)
        else
          return (ds ++ repeat '0')

-- | Parse hex string
parseHexString :: Parser ByteString
parseHexString = do
  void $ P.char '<'
  str <- many takeHex
  void $ P.char '>'
  return $ BS.pack str
  where
  takeHex = do
    ch1 <- P.satisfy isHexDigit
    ch2 <- P.satisfy isHexDigit
    return $ fromIntegral $ digitToInt ch1 * 16 + digitToInt ch2

-- | Parse a reference
parseRef :: Parser Ref
parseRef = do
  obj <- P.decimal
  P.skipSpace
  gen <- P.decimal
  P.skipSpace
  void $ P.char 'R'
  return $ R obj gen

-- | Parse a name
parseName :: Parser Name
parseName = do
  void $ P.char '/'
  -- XXX: escaping
  bs <- P.takeWhile1 isRegularChar
  either fail return $
    Name.make bs

-- | Whether the character can appear in 'Name'
isRegularChar :: Char -> Bool
isRegularChar = (`notElem` "[]()/<>{}% \n\r")

-- | Parse bool value
parseBool :: Parser Bool
parseBool = P.choice [
  P.string "true" >> return True,
  P.string "false" >> return False
  ]

-- | Consumes input till stream's data
--
-- Use 'parseDict' then 'parseTillStreamData'
-- to determine whether the object is dictionary or stream.
-- If 'parseTillStreamData' fails, then it is a dictionary.
-- Otherwise it is stream, and current position in input data
-- will point to stream's data start
--
-- >>> parse (parseDict >>= \dict -> parseTillStreamData >> return dict) "<</Key 123>>\nstream\n1234\nendstream"
-- Done "1234\nendstream" Dict [(Name "Key",ONumber (NumInt 123))]
parseTillStreamData :: Parser ()
parseTillStreamData = do
  P.skipSpace
  void $ P.string "stream"
  endOfLine

-- | Parse any 'Object' except 'Stream'
-- because for 'Stream' we need offset of data in file
--
-- >>> parseOnly parseObject "/Name"
-- Right (OName (Name "Name"))
parseObject :: Parser (Object ())
parseObject = do
  P.skipSpace
  P.choice [
    const ONull <$> P.string "null",
    OName <$> parseName,
    OBoolean <$> parseBool,
    ODict <$> parseDict,
    OArray <$> parseArray,
    OStr <$> parseString,
    OStr <$> parseHexString,
    ORef <$> parseRef,
    ONumber <$> parseNumber
    ]

-- | Parse object. Input position should point
-- to offset defined in XRef
--
-- >>> parseOnly parseIndirectObject "1 2 obj\n12"
-- Right (Ref 1 2,ONumber (NumInt 12))
parseIndirectObject :: Parser (Ref, Object ())
parseIndirectObject = do
  P.skipSpace
  index <- P.decimal :: Parser Int
  P.skipSpace
  gen <- P.decimal :: Parser Int
  P.skipSpace
  void $ P.string "obj"
  P.skipSpace
  obj <- parseObject
  let ref = R index gen
  case obj of
    ODict d -> P.choice [
      parseTillStreamData >> return (ref, OStream $ S d ()),
      return (ref, ODict d)
      ]
    _ -> return (ref, obj)
