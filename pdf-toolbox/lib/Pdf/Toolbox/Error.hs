
-- | Error used by API

module Pdf.Toolbox.Error
(
  PdfError(..),
  PdfE,
  annotateError,
  annotatingError,
  tryIO
)
where

import Control.Error hiding (tryIO)
import qualified Control.Error as Error
import Control.Monad.IO.Class

-- | Errors
data PdfError =
  -- | Parser error
  ParseError [String] String |
  IOError IOError |
  AnnotatedError String PdfError |
  -- | Something unexpected
  UnexpectedError String
  deriving Show

-- | API uses this for error handling
type PdfE m = EitherT PdfError m

-- | Wrap any 'PdfError' into 'AnnotatedError'
--
-- Usefull when you want to add high-level description to
-- error, returned by low-level function
annotateError :: Monad m => String -> PdfE m a -> PdfE m a
annotateError ann = fmapLT (AnnotatedError ann)

-- | 'annotateError' with fliped arguments
annotatingError :: Monad m => PdfE m a -> String -> PdfE m a
annotatingError = flip annotateError

-- | Catch exception if any and convert to 'IOError'
tryIO :: MonadIO m => IO a -> PdfE m a
tryIO action = fmapLT IOError $ Error.tryIO action
