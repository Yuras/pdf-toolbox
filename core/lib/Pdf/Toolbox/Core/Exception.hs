{-# LANGUAGE DeriveDataTypeable #-}

-- | Exceptions and utilities
module Pdf.Toolbox.Core.Exception
(
  Corrupted(..),
  Unexpected(..),
  sure,
  message
)
where

import Data.Typeable
import Control.Exception hiding (throw)

-- | File is corrupted
--
-- Contains general message and a list of details
data Corrupted = Corrupted String [String]
  deriving (Show, Typeable)

instance Exception Corrupted where

-- | Something unexpected occurs, probably API missuse
data Unexpected = Unexpected String [String]
  deriving (Show, Typeable)

instance Exception Unexpected where

-- | We are sure it is 'Right'. Otherwise 'Corripted' is thrown
sure :: Either String a -> IO a
sure (Right a) = return a
sure (Left err) = throwIO (Corrupted err [])

-- | Catch 'Corrupted' and 'Unexpected'
-- and add a message to it before rethrowing
message :: String -> IO a -> IO a
message msg a = a `catches`
  [ Handler $ \(Corrupted err msgs) -> throwIO (Corrupted msg (err : msgs))
  , Handler $ \(Unexpected err msgs) -> throwIO (Unexpected msg (err : msgs))
  ]
