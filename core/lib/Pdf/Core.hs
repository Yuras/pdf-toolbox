
-- | Low level API for parsing PDF file.
--
-- See "Pdf.Core.Writer" for basic API for writing new PDF file or
-- incrementally updating existing one.

module Pdf.Core
( File
, withPdfFile
, lastTrailer
, findObject
, Object(..)
, Name
, Dict
, Array
, Ref(..)
, Stream(..)
, streamContent
, EncryptionStatus(..)
, encryptionStatus
, setUserPassword
, defaultUserPassword
)
where

import Pdf.Core.Object
import Pdf.Core.File
import Pdf.Core.Encryption
