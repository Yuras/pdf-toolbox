
-- | Utils

module Pdf.Toolbox.Core.Parsers.Util
(
  endOfLine
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
