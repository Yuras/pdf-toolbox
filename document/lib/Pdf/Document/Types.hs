
-- | Various types

module Pdf.Document.Types
(
  Rectangle(..),
  rectangleFromArray
)
where

import Pdf.Core
import Pdf.Core.Util
import Pdf.Core.Object.Util

import qualified Data.Vector as Vector

-- | Rectangle
data Rectangle a = Rectangle a a a a
  deriving Show

-- | Create rectangle form an array of 4 numbers
rectangleFromArray :: Array -> Either String (Rectangle Double)
rectangleFromArray arr = do
  res <- mapM realValue (Vector.toList arr)
      `notice` "Rectangle should contain real values"
  case res of
    [a, b, c, d] -> return $ Rectangle a b c d
    _ -> Left ("rectangleFromArray: " ++ show arr)
