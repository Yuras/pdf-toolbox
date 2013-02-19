
-- | Incremental updates
--
-- Update PDF file by appending a \"patch\" to the end of the file.
-- Suboptimal implementation

module Pdf.Toolbox.Core.IncrementalUpdate
(
  buildPatch
)
where

import Data.Int
import Data.Monoid
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Builder

buildXRefSection :: [(Int64, Ref, Bool)] -> Builder
buildXRefSection [] = error "impossible"
buildXRefSection s@((_, Ref i _, _):_) =
  intDec i `mappend`
  char7 ' ' `mappend`
  intDec (length s) `mappend`
  char7 '\n' `mappend`
  loop s
  where
  loop ((off, Ref _ gen, exists):rest) =
    buildFixed 10 '0' off `mappend`
    char7 ' ' `mappend`
    buildFixed 5 '0' gen `mappend`
    char7 ' ' `mappend`
    char7 (if exists then 'n' else 'f') `mappend`
    string7 "\r\n" `mappend`
    loop rest
  loop [] = mempty

buildFixed :: Show a => Int -> Char -> a -> Builder
buildFixed len c i =
  let v = take len $ show i
      l = length v
  in string7 $ replicate (len - l) c ++ v

buildXRefTable :: [(Int64, Ref, Bool)] -> Builder
buildXRefTable entries =
  string7 "xref\n" `mappend`
  mconcat (map buildXRefSection $ sections entries)
  where
  sections :: [(Int64, Ref, Bool)] -> [[(Int64, Ref, Bool)]]
  sections [] = []
  sections xs = let (s, rest) = section xs in s : sections rest
  section [] = error "impossible"
  section (x:xs) = let (_, Ref i _, _) = x in go (i + 1) [x] xs
    where
    go _ res [] = (reverse res, [])
    go i res (y:ys) =
      let (_, Ref i' _, _) = y
      in if i == i'
          then go (i + 1) (y : res) ys
          else (reverse res, y:ys)

-- | Build a patch
--
-- Supply 'Nothing' and the original 'Ref' to delete the object.
-- Supply updated object and the original 'Ref' to modify the object.
-- Supply new object and new 'Ref' to add object (the new ref should
-- be allocated from the free ref list)
--
-- Ensure that the trailer everything necessary. Usually it sould be
-- the original trailer, except the \"Prev\" value should be updated
-- to point to the orinal xref
buildPatch :: [(Maybe (Object BSL.ByteString), Ref)]  -- ^ Object to add/modify
           -> Int64                                   -- ^ Current file size
           -> Dict                                    -- ^ New trailer
           -> Builder                                 -- ^ The patch to be appended to the file
buildPatch objects start trailer =
  objs `mappend`
  buildXRefTable entries `mappend`
  string7 "trailer\n" `mappend`
  buildDict trailer `mappend`
  string7 "\nstartxref\n" `mappend`
  int64Dec (start + fromIntegral (BSL.length $ toLazyByteString objs)) `mappend`
  string7 "\n%%EOF\n"
  where
  objs = mconcat builders
  (builders, entries) = unzip $ prepareObjects start objects
  prepareObjects :: Int64 -> [(Maybe (Object BSL.ByteString), Ref)] -> [(Builder, (Int64, Ref, Bool))]
  prepareObjects _ [] = []
  prepareObjects off ((Nothing, ref):xs) = (mempty, (0, ref, False)) : prepareObjects off xs
  prepareObjects off ((Just o, ref):xs) =
    let bs = toLazyByteString (buildIndirectObject ref o)
        len = fromIntegral $ BSL.length bs
    in (lazyByteString bs, (off, ref, True)) : prepareObjects (off + len) xs
