
-- | Various types

module Pdf.Toolbox.Document.Types
(
  Rectangle(..),
  rectangleFromArray
)
where

import Pdf.Toolbox.Core

-- | Rectangle
data Rectangle a = Rectangle a a a a
  deriving Show

-- | Create rectangle form an array of 4 numbers
rectangleFromArray :: Monad m => Array -> PdfE m (Rectangle Double)
rectangleFromArray (Array [a', b', c', d']) = do
  a <- fromObject a' >>= realValue
  b <- fromObject b' >>= realValue
  c <- fromObject c' >>= realValue
  d <- fromObject d' >>= realValue
  return $ Rectangle a b c d
rectangleFromArray array = throwE $ UnexpectedError $ "rectangleFromArray: " ++ show array
