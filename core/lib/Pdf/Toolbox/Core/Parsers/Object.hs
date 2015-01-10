{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | This module contains parsers for pdf objects

module Pdf.Toolbox.Core.Parsers.Object
(
  -- * Parse any object
  parseObject,
  -- * Parse object of specific type
  parseDict,
  parseArray,
  parseName,
  parseStr,
  parseHexStr,
  parseRef,
  parseNumber,
  parseBoolean,
  -- * Other
  parseTillStreamData,
  parseIndirectObject,
  isRegularChar
)
where

import Data.Char
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P

#if MIN_VERSION_attoparsec(0, 12, 0)
import qualified Data.Scientific as Scientific
#endif

import Control.Applicative
import Control.Monad

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Parsers.Util

-- |
-- >>> parseOnly parseDict "<</Key1(some string)/Key2 123>>"
-- Right (Dict [(Name "Key1",OStr (Str "some string")),(Name "Key2",ONumber (NumInt 123))])
parseDict :: Parser Dict
parseDict = do
  _ <- P.string "<<"
  dict <- many parseKey
  P.skipSpace
  _ <- P.string ">>"
  return $ Dict dict

parseKey :: Parser (Name, Object ())
parseKey = do
  P.skipSpace
  key <- parseName
  val <- parseObject
  return (key, val)

-- |
-- >>> parseOnly parseArray "[1 (string) /Name []]"
-- Right (Array [ONumber (NumInt 1),OStr (Str "string"),OName (Name "Name"),OArray (Array [])])
parseArray :: Parser Array
parseArray = do
  _ <- P.char '['
  array <- many parseObject
  P.skipSpace
  _ <- P.char ']'
  return $ Array array

-- |
-- >>> parseOnly parseNumber "123"
-- Right (NumInt 123)
-- >>> parseOnly parseNumber "12.3"
-- Right (NumReal 12.3)
-- >>> parseOnly parseNumber ".01"
-- Right (NumReal 1.0e-2)
parseNumber :: Parser Number
parseNumber = P.choice [
  number,
  NumReal <$>
    (P.signed
      $ read
      . ("0."++)
      . BS8.unpack <$>
        (P.char '.' >> P.takeWhile1 isDigit))
  ]
  where
#if MIN_VERSION_attoparsec(0, 12, 0)
  number = toNum <$> P.scientific
  toNum = either NumReal NumInt . Scientific.floatingOrInteger
#else
  number = toNum <$> P.number
  toNum (P.I i) = NumInt $ fromIntegral i
  toNum (P.D d) = NumReal d
#endif

-- |
-- >>> parseOnly parseStr "(hello)"
-- Right (Str "hello")
parseStr :: Parser Str
parseStr = do
  _ <- P.char '('
  str <- takeStr 0 []
  return $ Str $ BS8.pack str
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

-- |
-- >>> parseOnly parseHexStr "<68656C6C6F>"
-- Right (Str "hello")
parseHexStr :: Parser Str
parseHexStr = do
  _ <- P.char '<'
  str <- many takeHex
  _ <- P.char '>'
  return $ Str $ BS.pack str
  where
  takeHex = do
    ch1 <- P.satisfy isHexDigit
    ch2 <- P.satisfy isHexDigit
    return $ fromIntegral $ digitToInt ch1 * 16 + digitToInt ch2

-- |
-- >>> parseOnly parseRef "0 2 R"
-- Right (Ref 0 2)
parseRef :: Parser Ref
parseRef = do
  obj <- P.decimal
  P.skipSpace
  gen <- P.decimal
  P.skipSpace
  _ <- P.char 'R'
  return $ Ref obj gen

-- |
-- >>> parseOnly parseName "/Name"
-- Right (Name "Name")
parseName :: Parser Name
parseName = do
  _ <- P.char '/'
  Name <$> P.takeWhile1 isRegularChar

-- | Whether the character can appear in 'Name'
isRegularChar :: Char -> Bool
isRegularChar = (`notElem` "[]()/<>{}% \n\r")

-- |
-- >>> parseOnly parseBoolean "true"
-- Right (Boolean True)
-- >>> parseOnly parseBoolean "false"
-- Right (Boolean False)
parseBoolean :: Parser Boolean
parseBoolean = Boolean <$> P.choice [
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
  _ <- P.string "stream"
  endOfLine

-- | It parses any 'Object' except 'Stream'
-- cos for 'Stream' we need offset of data in file
--
-- >>> parseOnly parseObject "/Name"
-- Right (OName (Name "Name"))
parseObject :: Parser (Object ())
parseObject = do
  P.skipSpace
  P.choice [
    const ONull <$> P.string "null",
    OName <$> parseName,
    OBoolean <$> parseBoolean,
    ODict <$> parseDict,
    OArray <$> parseArray,
    OStr <$> parseStr,
    OStr <$> parseHexStr,
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
  _ <- P.string "obj"
  P.skipSpace
  obj <- parseObject
  let ref = Ref index gen
  case obj of
    ODict d -> P.choice [
      parseTillStreamData >> return (ref, OStream $ Stream d ()),
      return (ref, ODict d)
      ]
    _ -> return (ref, obj)
