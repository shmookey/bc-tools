module Munt.Primitives where

import qualified Crypto.Hash as H
import qualified Crypto.Cipher.Types as Cipher
import qualified Crypto.Error as CError
import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.DES as DES
import qualified Crypto.Cipher.Blowfish as Blowfish
import qualified Crypto.Cipher.TripleDES as TripleDES
import qualified Crypto.Cipher.Camellia as Camellia
import qualified Data.ASN1.Encoding as AsnE
import qualified Data.ASN1.BinaryEncoding as AsnBE
import qualified Data.Bits as Bits
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit as C
import qualified Data.Digits as Digits
import qualified Data.Hex as Hex
import qualified Data.Map as Map

import Data.Bits (Bits, (.|.), (.&.), shiftL, shiftR, xor)
import Data.Conduit (Conduit, (=$=))
import Data.List (intercalate, isPrefixOf)
import Text.Printf (printf)

import Munt.Types

roll :: B8.ByteString -> Integer
roll = B.foldl' unstep 0 . B.reverse
  where unstep a b = a `shiftL` 8 .|. fromIntegral b

roll' :: B8.ByteString -> Int
roll' = fromInteger . roll

unroll :: (Integral a, Bits a) => a -> B.ByteString
unroll = B.unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

filterRight :: Show a => Conduit (Either a b) IO b
filterRight = CL.mapM $ \x -> case x of Right x -> return x
                                        Left e  -> fail $ show e

sconcat  = CL.map B.concat
stake    = CL.take . roll'

eat :: Int -> ([Blob] -> Blob) -> [Blob] -> Transform
eat n f args = let pre = take n args
                   rem = n - (length args)
               in (CL.sequence $ CL.take rem) =$= CL.map (\xs -> f (pre ++ xs))

eat' :: Int -> ([B8.ByteString] -> [B8.ByteString]) -> [Blob] -> Transform
eat' n f args = let pre = take n args
                    rem = n - (length args)
                in (CL.sequence $ CL.take rem) =$= CL.concatMap (\xs -> f (pre ++ xs))

cipher :: Cipher.Cipher a => Blob -> a
cipher k = CError.throwCryptoError $ Cipher.cipherInit k

ecbEncrypt f = eat 2 $ \[k, c] -> Cipher.ecbEncrypt (f k) c
ecbDecrypt f = eat 2 $ \[k, c] -> Cipher.ecbDecrypt (f k) c

hash x = BA.convert . H.hashWith x
hmap :: H.HashAlgorithm a => a -> Conduit B8.ByteString IO B8.ByteString
hmap   = CL.map . hash

fun :: Transform -> [Blob] -> Transform
fun f args = f

primitive :: String -> [Blob] -> IO Transform
primitive x args = case lookupPrimitive x of
  [(_,f)] -> return $ f args
  []      -> fail $ "Undefined function: " ++ x
  xs      -> fail $ "Ambiguous function reference: " ++ x 
                 ++ ". Possible candidates: "
                 ++ (intercalate " " $ map fst xs)

lookupPrimitive :: String -> [(String, [Blob] -> Transform)]
lookupPrimitive x = 
  case Map.lookup x primitives of
    Just f -> [(x, f)]
    Nothing -> Map.toList $ Map.filterWithKey prefix primitives
  where prefix name f = isPrefixOf x name

primitives :: Map.Map String ([Blob] -> Transform)
primitives = Map.fromList
 [ ("trace"     , fun $ CC.iterM (putStrLn . printf "TRACE: %s" . show))
 , ("pasn1d"    , fun $ (CL.map $ AsnE.decodeASN1' AsnBE.DER)
              =$= filterRight
              =$= CL.concatMap (map (B8.pack . show)))
  
   -- List operations
 , ("len"       , fun $ CL.map $ unroll . toInteger . B8.length)
 , ("reverse"   , fun $ CL.map B8.reverse)
 , ("head"      , fun $ CL.map $ B8.singleton . B8.head)
 , ("tail"      , fun $ CL.map B8.tail)
 , ("init"      , fun $ CL.map B8.init)
 , ("last"      , fun $ CL.map $ B8.singleton . B8.last)
 , ("take"      , eat 2 $ \[n, xs]  -> B8.take (fromInteger $ roll n) xs)
 , ("drop"      , eat 2 $ \[n, xs]  -> B8.drop (fromInteger $ roll n) xs)
 , ("prepend"   , eat 2 $ \[xs, ys] -> B8.concat [xs, ys])
 , ("append"    , eat 2 $ \[xs, ys] -> B8.concat [ys, xs])
  
   -- Stream operations
 , ("id"        , fun $ CL.map id)
 , ("after"     , \args -> CL.concatMap (:args))
 , ("before"    , \args -> CL.concatMap (\x -> args ++ [x]))
 , ("bytes"     , fun $ CL.concatMap $ map B8.singleton . B8.unpack)
 , ("consume"   , fun $ CL.sequence CL.consume =$= CL.map B8.concat)
 , ("count"     , fun $ CL.sequence CL.consume =$= CL.map (unroll . length))
 , ("dup"       , fun $ CL.concatMap $ replicate 2)
 , ("lines"     , fun $ CL.concatMap B8.lines)
 , ("repeat"    , eat' 2 $ \[n, x] -> replicate (fromInteger $ roll n) x)
 , ("filter"    , fun $ CL.filter (const False))
 , ("flip"      , eat' 2 $ \[x, y] -> [y, x])
 , ("unlines"   , fun $ CL.sequence CL.consume =$= CL.map (B8.init . B8.unlines))
 , ("words"     , fun $ CL.concatMap B8.words)
 , ("unwords"   , fun $ CL.sequence CL.consume =$= CL.map B8.unwords)
 , ("concat"    , \args -> case args of
                      [n] -> CL.sequence (stake n) =$= sconcat
                      _   -> CL.sequence (CL.take 1 >>= stake . head) =$= sconcat)
  
   -- Number formatting
 , ("hex"       , fun $ CL.map Hex.hex)
 , ("unhex"     , fun $ CL.mapM Hex.unhex)
 , ("dec"       , fun $ CL.map $ B8.pack . show . roll)
 , ("undec"     , fun $ CL.map $ unroll . toInteger . read . B8.unpack)
 , ("bin"       , fun $ CL.map $ B.concatMap (B8.pack . printf "%0.8b"))
 , ("unbin"     , fun $ CL.map $ unroll . Digits.unDigits 2 . map (toInteger . read) . map (:[]) . B8.unpack)
  
   -- Math
 , ("+"         , eat 2 $ unroll . (\[d, x] -> x + d)     . map roll)
 , ("-"         , eat 2 $ unroll . (\[d, x] -> x - d)     . map roll)
 , ("*"         , eat 2 $ unroll . (\[d, x] -> x * d)     . map roll)
 , ("^"         , eat 2 $ unroll . (\[d, x] -> x ^ d)     . map roll)
 , ("/"         , eat 2 $ unroll . (\[d, x] -> x `div` d) . map roll)
 , ("%"         , eat 2 $ unroll . (\[d, x] -> x `mod` d) . map roll)
  
   -- Bitwise
 , ("and"       , eat 2 $ unroll . (\[x, y] -> x .&. y)   . map roll)
 , ("or"        , eat 2 $ unroll . (\[x, y] -> x .|. y)   . map roll)
 , ("xor"       , eat 2 $ unroll . (\[x, y] -> x `xor` y) . map roll)
 , ("not"       , fun $ CL.map $ unroll . Bits.complement . roll)
 , ("rsh"       , eat 2 $ unroll . (\[n, x] -> x `shiftR` (fromInteger n)) . map roll)
 , ("lsh"       , eat 2 $ unroll . (\[n, x] -> x `shiftL` (fromInteger n)) . map roll)
  
   -- Encoding
 , ("b64e"      , fun $ CL.map B64.encode )
 , ("b64d"      , fun $ CL.map B64.decodeLenient)
  
   -- Cipher
 , ("aes128e"   , ecbEncrypt (cipher :: Cipher AES.AES128))
 , ("aes128d"   , ecbDecrypt (cipher :: Cipher AES.AES128))
 , ("aes192e"   , ecbEncrypt (cipher :: Cipher AES.AES192))
 , ("aes192d"   , ecbDecrypt (cipher :: Cipher AES.AES192))
 , ("aes256e"   , ecbEncrypt (cipher :: Cipher AES.AES256))
 , ("aes256d"   , ecbDecrypt (cipher :: Cipher AES.AES256))
 , ("cam128e"   , ecbEncrypt (cipher :: Cipher Camellia.Camellia128) )
 , ("cam128d"   , ecbDecrypt (cipher :: Cipher Camellia.Camellia128) )
 , ("dese"      , ecbEncrypt (cipher :: Cipher DES.DES) )
 , ("desd"      , ecbDecrypt (cipher :: Cipher DES.DES) )
 , ("deseee3e"  , ecbEncrypt (cipher :: Cipher TripleDES.DES_EEE3) )
 , ("deseee3d"  , ecbDecrypt (cipher :: Cipher TripleDES.DES_EEE3) )
 , ("desede3e"  , ecbEncrypt (cipher :: Cipher TripleDES.DES_EDE3) )
 , ("desede3d"  , ecbDecrypt (cipher :: Cipher TripleDES.DES_EDE3) )
 , ("deseee2e"  , ecbEncrypt (cipher :: Cipher TripleDES.DES_EEE2) )
 , ("deseee2d"  , ecbDecrypt (cipher :: Cipher TripleDES.DES_EEE2) )
 , ("desede2e"  , ecbEncrypt (cipher :: Cipher TripleDES.DES_EDE2) )
 , ("desede2d"  , ecbDecrypt (cipher :: Cipher TripleDES.DES_EDE2) )
 , ("bfe"       , ecbEncrypt (cipher :: Cipher Blowfish.Blowfish) )
 , ("bfd"       , ecbDecrypt (cipher :: Cipher Blowfish.Blowfish) )
 , ("bf64e"     , ecbEncrypt (cipher :: Cipher Blowfish.Blowfish64) )
 , ("bf64d"     , ecbDecrypt (cipher :: Cipher Blowfish.Blowfish64) )
 , ("bf128e"    , ecbEncrypt (cipher :: Cipher Blowfish.Blowfish128) )
 , ("bf128d"    , ecbDecrypt (cipher :: Cipher Blowfish.Blowfish128) )
 , ("bf256e"    , ecbEncrypt (cipher :: Cipher Blowfish.Blowfish256) )
 , ("bf256d"    , ecbDecrypt (cipher :: Cipher Blowfish.Blowfish256) )
 , ("bf448e"    , ecbEncrypt (cipher :: Cipher Blowfish.Blowfish448) )
 , ("bf448d"    , ecbDecrypt (cipher :: Cipher Blowfish.Blowfish448))
  
   -- Hashes
 , ("blake2s256"  , fun $ hmap H.Blake2s_256)
 , ("blake2s224"  , fun $ hmap H.Blake2s_224)
 , ("blake2sp256" , fun $ hmap H.Blake2sp_256)
 , ("blake2sp224" , fun $ hmap H.Blake2sp_224)
 , ("blake2b512"  , fun $ hmap H.Blake2b_512)
 , ("blake2bp512" , fun $ hmap H.Blake2bp_512)
 , ("md2"         , fun $ hmap H.MD2)
 , ("md4"         , fun $ hmap H.MD4)
 , ("md5"         , fun $ hmap H.MD5)
 , ("sha1"        , fun $ hmap H.SHA1)
 , ("sha224"      , fun $ hmap H.SHA224)
 , ("sha256"      , fun $ hmap H.SHA256)
 , ("sha384"      , fun $ hmap H.SHA384)
 , ("sha512"      , fun $ hmap H.SHA512)
 , ("sha512t256"  , fun $ hmap H.SHA512t_256)
 , ("sha512t224"  , fun $ hmap H.SHA512t_224)
 , ("sha3512"     , fun $ hmap H.SHA3_512)
 , ("sha3384"     , fun $ hmap H.SHA3_384)
 , ("sha3256"     , fun $ hmap H.SHA3_256)
 , ("sha3224"     , fun $ hmap H.SHA3_224)
 , ("keccak512"   , fun $ hmap H.Keccak_512)
 , ("keccak384"   , fun $ hmap H.Keccak_384)
 , ("keccak256"   , fun $ hmap H.Keccak_256)
 , ("keccak224"   , fun $ hmap H.Keccak_224)
 , ("ripemd160"   , fun $ hmap H.RIPEMD160)
 , ("skein256256" , fun $ hmap H.Skein256_256)
 , ("skein256224" , fun $ hmap H.Skein256_224)
 , ("skein512512" , fun $ hmap H.Skein512_512)
 , ("skein512384" , fun $ hmap H.Skein512_384)
 , ("skein512256" , fun $ hmap H.Skein512_256)
 , ("skein512224" , fun $ hmap H.Skein512_224)
 , ("whirlpool"   , fun $ hmap H.Whirlpool)
 ]
