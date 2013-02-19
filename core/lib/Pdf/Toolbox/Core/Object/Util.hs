
-- | Utils relayted to pdf objects

module Pdf.Toolbox.Core.Object.Util
(
  -- * Casting pdf objects
  FromObject(..),
  toNumber,
  toBoolean,
  toName,
  toDict,
  toArray,
  toStr,
  toRef,
  toStream,
  mapObject,
  -- * Dictionary
  lookupDict,
  lookupDict',
  setValueForKey,
  deleteValueForKey,
  -- * Number
  intValue,
  realValue
)
where

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Error

lookupDict :: Monad m => Name -> Dict -> PdfE m (Object ())
lookupDict key (Dict d) =
  case lookup key d of
    Just o -> right o
    Nothing -> left $ UnexpectedError $ "Key not found: " ++ show key ++ " " ++ show d

lookupDict' :: Name -> Dict -> Maybe (Object ())
lookupDict' key (Dict d) = lookup key d

deleteValueForKey :: Name -> Dict -> Dict
deleteValueForKey key (Dict vals) = Dict $ go vals
  where
  go [] = []
  go (x@(k, _) : xs)
    | k == key = go xs
    | otherwise = x : go xs

setValueForKey :: Name -> Object () -> Dict -> Dict
setValueForKey key val dict = Dict $ (key, val) : vals
  where
  Dict vals = deleteValueForKey key dict

intValue :: Monad m => Number -> PdfE m Int
intValue (NumInt i) = right i
intValue (NumReal r) = left $ UnexpectedError $ "Integer expected, but real received: " ++ show r

realValue :: Monad m => Number -> PdfE m Double
realValue (NumReal r) = right r
realValue (NumInt i) = right $ fromIntegral i

toNumber :: (Show a, Monad m) => Object a -> PdfE m Number
toNumber (ONumber n) = right n
toNumber o = left $ UnexpectedError $ "Can't cast object to Number: " ++ show o

toBoolean :: (Show a, Monad m) => Object a -> PdfE m Boolean
toBoolean (OBoolean b) = right b
toBoolean o = left $ UnexpectedError $ "Can't cast object to Boolean: " ++ show o

toName :: (Show a, Monad m) => Object a -> PdfE m Name
toName (OName n) = right n
toName o = left $ UnexpectedError $ "Can't cast object to Name: " ++ show o

toDict :: (Show a, Monad m) => Object a -> PdfE m Dict
toDict (ODict d) = right d
toDict o = left $ UnexpectedError $ "Can't cast object to Dict: " ++ show o

toStr :: (Show a, Monad m) => Object a -> PdfE m Str
toStr (OStr s) = right s
toStr o = left $ UnexpectedError $ "Can't cast object to Str: " ++ show o

toRef :: (Show a, Monad m) => Object a -> PdfE m Ref
toRef (ORef r) = right r
toRef o = left $ UnexpectedError $ "Can't cast object to Ref: " ++ show o

toArray :: (Show a, Monad m) => Object a -> PdfE m Array
toArray (OArray a) = right a
toArray o = left $ UnexpectedError $ "Can't cast object to Array: " ++ show o

toStream :: (Show a, Monad m) => Object a -> PdfE m (Stream a)
toStream (OStream s) = right s
toStream o = left $ UnexpectedError $ "Can't cast object to Stream: " ++ show o

-- | Apply function to all stream contents
mapObject :: (a -> b) -> Object a -> Object b
mapObject f o =
  case o of
    ONumber n -> ONumber n
    OBoolean b -> OBoolean b
    OName n -> OName n
    ODict d -> ODict d
    OArray a -> OArray a
    OStr s -> OStr s
    OStream (Stream d a) -> OStream (Stream d $ f a)
    ORef r -> ORef r
    ONull -> ONull

-- | Allows you to cast 'Object' to specific type
class FromObject c where
  fromObject :: (Show a, Monad m) => Object a -> PdfE m c

instance FromObject Number where
  fromObject = toNumber

instance FromObject Boolean where
  fromObject = toBoolean

instance FromObject Name where
  fromObject = toName

instance FromObject Dict where
  fromObject = toDict

instance FromObject Str where
  fromObject = toStr

instance FromObject Ref where
  fromObject = toRef

instance FromObject Array where
  fromObject = toArray
