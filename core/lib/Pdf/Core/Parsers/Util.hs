
-- | Utils

module Pdf.Core.Parsers.Util
(
  endOfLine
, skipSpacesAndComments
)
where

import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative (many)

-- | In pdf file EOL could be \"\\n\", \"\\r\" or \"\\n\\r\"
--
-- Also space (0x20) is usually ok before EOL
endOfLine :: Parser ()
endOfLine = do
  _ <- many $ P.char ' '
  P.choice [
    P.endOfLine, -- it already handles both the \n and \n\r
    P.char '\r' >>= const (return ())
    ]

skipSpacesAndComments :: Parser ()
skipSpacesAndComments = do
  P.skipSpace
  _ <- many (skipComment *> skipSpacesAndComments)
  return ()
 where
   skipComment :: Parser ()
   skipComment = do
     _ <- P.char '%'
     P.skipWhile notEndOfLine
     endOfLine
   notEndOfLine '\r' = False
   notEndOfLine '\n' = False
   notEndOfLine _    = True
                    
