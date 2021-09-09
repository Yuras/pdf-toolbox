
-- | Compound data structures from sec. 7.9 of PDF32000:2008

module Pdf.Core.Types
(
  Rectangle(..),
  rectangleFromArray,
  rectangleToArray
)
where

import qualified Data.Scientific as Scientific
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

rectangleToArray :: Rectangle Double -> Array
rectangleToArray (Rectangle a b c d) =
  Vector.fromList . map (Number . Scientific.fromFloatDigits) $ [a, b, c, d]
