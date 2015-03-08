{-# LANGUAGE OverloadedStrings #-}

-- | Basic support for encrypted PDF documents

module Pdf.Toolbox.Document.Encryption
(
  Decryptor,
  DecryptorScope(..),
  defaultUserPassword,
  mkStandardDecryptor,
  decryptObject
)
where

import qualified Data.Traversable as Traversable
import Data.Bits (xor)
import Data.IORef
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Builder
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Applicative
import Control.Monad
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified Crypto.Cipher.RC4 as RC4
import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Padding as Padding

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Util

-- | Encryption handler may specify different encryption keys for strings
-- and streams
data DecryptorScope
  = DecryptString
  | DecryptStream

-- | Decrypt input stream
type Decryptor
  =  Ref
  -> DecryptorScope
  -> InputStream ByteString
  -> IO (InputStream ByteString)

-- | Decrypt object with the decryptor
decryptObject :: Decryptor -> Ref -> Object a -> IO (Object a)
decryptObject decryptor ref (OStr str)
  = OStr <$> decryptStr decryptor ref str
decryptObject decryptor ref (ODict dict)
  = ODict <$> decryptDict decryptor ref dict
decryptObject decryptor ref (OArray arr)
  = OArray <$> decryptArray decryptor ref arr
decryptObject _ _ o = return o

decryptStr :: Decryptor -> Ref -> ByteString -> IO ByteString
decryptStr decryptor ref str = do
  is <- Streams.fromList [str]
  res <- decryptor ref DecryptString is >>= Streams.toList
  return $ BS.concat res

decryptDict :: Decryptor -> Ref -> Dict -> IO Dict
decryptDict decryptor ref vals = Traversable.forM vals $
  decryptObject decryptor ref

decryptArray :: Decryptor -> Ref -> Array -> IO Array
decryptArray decryptor ref vals = Vector.forM vals decr
  where
  decr = decryptObject decryptor ref

-- | The default user password
defaultUserPassword :: ByteString
defaultUserPassword = BS.pack [
  0x28, 0xBF, 0x4E, 0x5E, 0x4E, 0x75, 0x8A, 0x41, 0x64, 0x00, 0x4E,
  0x56, 0xFF, 0xFA, 0x01, 0x08, 0x2E, 0x2E, 0x00, 0xB6, 0xD0, 0x68,
  0x3E, 0x80, 0x2F, 0x0C, 0xA9, 0xFE, 0x64, 0x53, 0x69, 0x7A
  ]

-- | Standard decryptor, RC4
mkStandardDecryptor :: Dict
                    -- ^ document trailer
                    -> Dict
                    -- ^ encryption dictionary
                    -> ByteString
                    -- ^ user password (32 bytes exactly,
                    -- see 7.6.3.3 Encryption Key Algorithm)
                    -> Either String (Maybe Decryptor)
mkStandardDecryptor tr enc pass = do
  filterType <-
    case HashMap.lookup "Filter" enc of
      Just o -> nameValue o `notice` "Filter should be a name"
      _ -> Left "Filter missing"
  unless (filterType == "Standard") $
    Left ("Unsupported encryption handler: " ++ show filterType)

  v <-
    case HashMap.lookup "V" enc of
      Just n -> intValue n `notice` "V should be an integer"
      _ -> Left "V is missing"

  if v == 4
    then mk4
    else mk12 v

  where
  mk12 v = do
    n <-
      case v of
        1 -> Right 5
        2 -> do
          case HashMap.lookup "Length" enc of
            Just o -> fmap (`div` 8) (intValue o
                        `notice` "Length should be an integer")
            Nothing -> Left "Length is missing"
        _ -> Left ("Unsuported encryption handler version: " ++ show v)

    ekey <- mkKey tr enc pass n
    ok <- verifyKey tr enc ekey
    return $
      if not ok
        then Nothing
        else Just $ \ref _ is -> mkDecryptor V2 ekey n ref is

  mk4 = do
    cryptoFilters <-
      case HashMap.lookup "CF" enc of
        Nothing -> Left "CF is missing in crypt handler V4"
        Just o -> dictValue o `notice` "CF should be a dictionary"
    keysMap <- Traversable.forM cryptoFilters $ \obj -> do
      dict <- dictValue obj `notice` "Crypto filter should be a dictionary"
      n <-
        case HashMap.lookup "Length" dict of
          Nothing -> Left "Crypto filter without Length"
          Just o -> intValue o `notice` "Crypto filter length should be int"
      algName <-
        case HashMap.lookup "CFM" dict of
          Nothing -> Left "CFM is missing"
          Just o -> nameValue o `notice` "CFM should be a name"
      alg <-
        case algName of
          "V2" -> return V2
          "AESV2" -> return AESV2
          _ -> Left $ "Unknown crypto method: " ++ show algName
      ekey <- mkKey tr enc pass n
      return (ekey, n, alg)

    (stdCfKey, _, _) <- HashMap.lookup "StdCF" keysMap
      `notice` "StdCF is missing"
    ok <- verifyKey tr enc stdCfKey
    if not ok
      then return Nothing

      else do
        strFName <- (HashMap.lookup "StrF" enc >>= nameValue)
          `notice` "StrF is missing"
        (strFKey, strFN, strFAlg) <- HashMap.lookup strFName keysMap
          `notice` ("Crypto filter not found: " ++ show strFName)

        stmFName <- (HashMap.lookup "StmF" enc >>= nameValue)
          `notice` "StmF is missing"
        (stmFKey, stmFN, stmFAlg) <- HashMap.lookup stmFName keysMap
          `notice` ("Crypto filter not found: " ++ show stmFName)

        return $ Just $ \ref scope is ->
          case scope of
            DecryptString -> mkDecryptor strFAlg strFKey strFN ref is
            DecryptStream -> mkDecryptor stmFAlg stmFKey stmFN ref is

mkKey :: Dict -> Dict -> ByteString -> Int -> Either String ByteString
mkKey tr enc pass n = do
  oVal <- do
    o <- HashMap.lookup "O" enc `notice` "O is missing"
    stringValue o `notice` "o should be a string"

  pVal <- do
    o <- HashMap.lookup "P" enc `notice` "P is missing"
    i <- intValue o `notice` "P should be an integer"
    Right . BS.pack . BSL.unpack . toLazyByteString
          . word32LE . fromIntegral $ i

  idVal <- do
    ids <- (HashMap.lookup "ID" tr >>= arrayValue)
        `notice` "ID should be an array"
    case (Vector.toList ids) of
      [] -> Left "ID array is empty"
      (x:_) -> stringValue x
                  `notice` "The first element if ID should be a string"

  rVal <- (HashMap.lookup "R" enc >>= intValue)
      `notice` "R should be an integer"

  encMD <-
    case HashMap.lookup "EncryptMetadata" enc of
      Nothing -> return True
      Just o -> boolValue o `notice` "EncryptMetadata should be a bool"

  let ekey' = BS.take n $ MD5.hash $ BS.concat [pass, oVal, pVal, idVal, pad]
      pad =
        if rVal < 4 || encMD
          then BS.empty
          else BS.pack (replicate 4 255)
      ekey =
        if rVal < 3
           then ekey'
           else foldl (\bs _ -> BS.take n $ MD5.hash bs)
                      ekey'
                      [1 :: Int .. 50]

  return ekey

verifyKey :: Dict -> Dict -> ByteString -> Either String Bool
verifyKey tr enc ekey = do
  rVal <- (HashMap.lookup "R" enc >>= intValue)
      `notice` "R should be an integer"

  idVal <- do
    ids <- (HashMap.lookup "ID" tr >>= arrayValue)
        `notice` "ID should be an array"
    case (Vector.toList ids) of
      [] -> Left "ID array is empty"
      (x:_) -> stringValue x
                  `notice` "The first element if ID should be a string"

  uVal <- (HashMap.lookup "U" enc >>= stringValue)
      `notice` "U should be a string"

  return $
    case rVal of
      2 ->
        let uVal' = snd $ RC4.combine (RC4.initCtx ekey)
                                      defaultUserPassword
        in uVal == uVal'
      _ ->
        let pass1 = snd $ RC4.combine (RC4.initCtx ekey)
                        $ BS.take 16 $ MD5.hash
                        $ BS.concat [defaultUserPassword, idVal]
            uVal' = loop 1 pass1
            loop 20 input = input
            loop i input = loop (i + 1) $ snd $ RC4.combine (RC4.initCtx
                                        $ BS.map (`xor` i) ekey) input
        in BS.take 16 uVal == BS.take 16 uVal'

data Algorithm
  = V2
  | AESV2
  deriving (Show)

mkDecryptor
  :: Algorithm
  -> ByteString
  -> Int
  -> Ref
  -> InputStream ByteString
  -> IO (InputStream ByteString)
mkDecryptor alg ekey n (Ref index gen) is = do
  let key = BS.take (16 `min` n + 5) $ MD5.hash $ BS.concat
        [ ekey
        , BS.pack $ take 3 $ BSL.unpack $ toLazyByteString
                  $ int32LE $ fromIntegral index
        , BS.pack $ take 2 $ BSL.unpack $ toLazyByteString
                  $ int32LE $ fromIntegral gen
        , salt alg
        ]
      salt V2 = ""
      salt AESV2 = "sAlT"

  case alg of
    V2 -> do
      ioRef <- newIORef $ RC4.initCtx key
      let readNext = do
            chunk <- Streams.read is
            case chunk of
              Nothing -> return Nothing
              Just c -> do
                ctx' <- readIORef ioRef
                let (ctx'', res) = RC4.combine ctx' c
                writeIORef ioRef ctx''
                return (Just res)
      Streams.makeInputStream readNext

    AESV2 -> do
      content <- BS.concat <$> Streams.toList is
      let initV = BS.take 16 content
          aes = AES.initAES key
          decrypted = AES.decryptCBC aes initV $ BS.drop 16 content
      Streams.fromByteString $ Padding.unpadPKCS5 decrypted
