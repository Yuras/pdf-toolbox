
-- | Various types

module Pdf.Toolbox.Document.Types
(
  Rectangle(..),
  rectangleFromArray
)
where

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Util

-- | Rectangle
data Rectangle a = Rectangle a a a a
  deriving Show

-- | Create rectangle form an array of 4 numbers
rectangleFromArray :: Array -> Either String (Rectangle Double)
rectangleFromArray (Array arr@[_, _, _, _]) = do
  [a, b, c, d] <- mapM realValue arr
      `notice` "Rectangle should contain real values"
  return $ Rectangle a b c d
rectangleFromArray array = Left ("rectangleFromArray: " ++ show array)
