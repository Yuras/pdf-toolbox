
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
  -- * Dictionary
  lookupDict,
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

intValue :: Monad m => Number -> PdfE m Int
intValue (NumInt i) = right i
intValue (NumReal r) = left $ UnexpectedError $ "Integer expected, but real received: " ++ show r

realValue :: Monad m => Number -> PdfE m Double
realValue (NumReal r) = right r
realValue (NumInt i) = left $ UnexpectedError $ "Real expected, but integer received: " ++ show i

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
