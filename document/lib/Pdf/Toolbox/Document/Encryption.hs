{-# LANGUAGE OverloadedStrings #-}

-- | Basic support for encrypted PDF documents

module Pdf.Toolbox.Document.Encryption
(
  Decryptor,
  defaultUserPassword,
  mkStandardDecryptor,
  decryptObject,
  DecryptorScope(..),
)
where

import Data.Bits (xor)
import Data.IORef
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Builder
import Control.Monad
import qualified System.IO.Streams as Streams
import qualified Crypto.Cipher.RC4 as RC4
import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Padding as Padding

import Pdf.Toolbox.Core

-- | Encryption handler may specify different encryption keys for strings
-- and streams
data DecryptorScope
  = DecryptString
  | DecryptStream

-- | Decrypt input stream
type Decryptor = Ref -> DecryptorScope -> IS -> IO IS

-- | Decrypt object with the decryptor
decryptObject :: (IS -> IO IS) -> Object a -> IO (Object a)
decryptObject decryptor (OStr str) = OStr `liftM` decryptStr decryptor str
decryptObject decryptor (ODict dict) = ODict `liftM` decryptDict decryptor dict
decryptObject _ o = return o

decryptStr :: (IS -> IO IS) -> Str -> IO Str
decryptStr decryptor (Str str) = do
  is <- Streams.fromList [str]
  res <- decryptor is >>= Streams.toList
  return $ Str $ BS.concat res

decryptDict :: (IS -> IO IS) -> Dict -> IO Dict
decryptDict decryptor (Dict vals) = Dict `liftM` forM vals decr
  where
  decr (key, val) = do
    res <- decryptObject decryptor val
    return (key, res)

-- | The default user password
defaultUserPassword :: ByteString
defaultUserPassword = BS.pack [
  0x28, 0xBF, 0x4E, 0x5E, 0x4E, 0x75, 0x8A, 0x41, 0x64, 0x00, 0x4E,
  0x56, 0xFF, 0xFA, 0x01, 0x08, 0x2E, 0x2E, 0x00, 0xB6, 0xD0, 0x68,
  0x3E, 0x80, 0x2F, 0x0C, 0xA9, 0xFE, 0x64, 0x53, 0x69, 0x7A
  ]

-- | Standard decryptor. RC4
mkStandardDecryptor :: Monad m
                    => Dict            -- ^ document trailer
                    -> Dict            -- ^ encryption dictionary
                    -> ByteString      -- ^ user password (32 bytes exactly, see 7.6.3.3 Encryption Key Algorithm)
                    -> PdfE m (Maybe Decryptor)
mkStandardDecryptor tr enc pass = do
  Name filterType <- lookupDict "Filter" enc >>= fromObject
  unless (filterType == "Standard") $ left $ UnexpectedError $ "Unsupported encryption handler: " ++ show filterType
  vVal <- lookupDict "V" enc >>= fromObject >>= intValue

  if vVal == 4
    then mk4
    else mk12 vVal

  where
  mk12 vVal = do
    n <- case vVal of
           1 -> return 5
           2 -> do
             len <- lookupDict "Length" enc >>= fromObject >>= intValue
             return $ len `div` 8
           _ -> left $ UnexpectedError $ "Unsuported encryption handler version: " ++ show vVal

    ekey <- mkKey tr enc pass n
    ok <- verifyKey tr enc ekey
    return $
      if not ok
        then Nothing
        else Just $ \ref _ is -> mkDecryptor V2 ekey n ref is

  mk4 = do
    Dict cryptoFilters <- lookupDict "CF" enc >>= fromObject

    keysMap <- forM cryptoFilters $ \(name, obj) -> do
      dict <- fromObject obj
      n <- lookupDict "Length" dict >>= fromObject >>= intValue
      algName <- lookupDict "CFM" dict >>= toName
      alg <-
        case algName of
          "V2" -> return V2
          "AESV2" -> return AESV2
          _ -> left $ UnexpectedError $ "Unknown crypto method: " ++ show algName
      ekey <- mkKey tr enc pass n
      return (name, (ekey, n, alg))

    (stdCfKey, _, _) <-
      case lookup "StdCF" keysMap of
        Nothing -> left $ UnexpectedError "StdCF is missing"
        Just v -> return v
    ok <- verifyKey tr enc stdCfKey
    if not ok
      then return Nothing

      else do
        strFName <- lookupDict "StrF" enc >>= toName
        (strFKey, strFN, strFAlg) <-
          case lookup strFName keysMap of
            Nothing -> left $ UnexpectedError $ "Crypto filter not found: " ++ show strFName
            Just v -> return v

        stmFName <- lookupDict "StmF" enc >>= toName
        (stmFKey, stmFN, stmFAlg) <-
          case lookup stmFName keysMap of
            Nothing -> left $ UnexpectedError $ "Crypto filter not found: " ++ show stmFName
            Just v -> return v

        return $ Just $ \ref scope is ->
          case scope of
            DecryptString -> mkDecryptor strFAlg strFKey strFN ref is
            DecryptStream -> mkDecryptor stmFAlg stmFKey stmFN ref is

mkKey :: Monad m => Dict -> Dict -> ByteString -> Int -> PdfE m ByteString
mkKey tr enc pass n = do
  Str oVal <- lookupDict "O" enc >>= fromObject
  pVal <- (BS.pack . BSL.unpack . toLazyByteString . word32LE . fromIntegral)
    `liftM` (lookupDict "P" enc >>= fromObject >>= intValue)
  Str idVal <- do
    Array ids <- lookupDict "ID" tr >>= fromObject
    case ids of
      [] -> left $ UnexpectedError $ "ID array is empty"
      (x:_) -> fromObject x
  rVal <- lookupDict "R" enc >>= fromObject >>= intValue

  encMD <-
    case lookupDict' "EncryptMetadata" enc of
      Nothing -> return True
      Just o -> do
        Boolean b <- toBoolean o
        return b

  let ekey' = BS.take n $ MD5.hash $ BS.concat [pass, oVal, pVal, idVal, pad]
      pad =
        if rVal < 4 || encMD
          then BS.empty
          else BS.pack (replicate 4 255)

  let ekey = if rVal < 3
               then ekey'
               else foldl (\bs _ -> BS.take n $ MD5.hash bs) ekey'  [1 :: Int .. 50]
  return ekey

verifyKey :: Monad m => Dict -> Dict -> ByteString -> PdfE m Bool
verifyKey tr enc ekey = do
  Str idVal <- do
    Array ids <- lookupDict "ID" tr >>= fromObject
    case ids of
      [] -> left $ UnexpectedError $ "ID array is empty"
      (x:_) -> fromObject x
  rVal <- lookupDict "R" enc >>= fromObject >>= intValue
  Str uVal <- lookupDict "U" enc >>= fromObject
  let ok =
        case rVal of
          2 ->
            let uVal' = snd $ RC4.combine (RC4.initCtx ekey) defaultUserPassword
            in uVal == uVal'
          _ ->
            let pass1 = snd $ RC4.combine (RC4.initCtx ekey) $ BS.take 16 $ MD5.hash $ BS.concat [defaultUserPassword, idVal]
                uVal' = loop 1 pass1
                loop 20 input = input
                loop i input = loop (i + 1) $ snd $ RC4.combine (RC4.initCtx $ BS.map (`xor` i) ekey) input
            in BS.take 16 uVal == BS.take 16 uVal'
  return ok

data Algorithm
  = V2
  | AESV2
  deriving (Show)

mkDecryptor
  :: Algorithm
  -> ByteString
  -> Int
  -> Ref
  -> IS
  -> IO IS
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
