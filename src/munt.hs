{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- munt - cryptographic function composition

import qualified Data.ASN1.Encoding as AsnE
import qualified Data.ASN1.BinaryEncoding as AsnBE
import qualified Data.List.Split as Split
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteArray as BA
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import qualified Data.Hex as Hex
import qualified Options.Applicative as Opts
import qualified Crypto.Hash as H
import Options.Applicative ((<>))
import qualified Options.Applicative.Help.Chunk as OAHC
import Data.Conduit (Source, Sink, Conduit, yield, ($$), (=$=))
import Control.Monad.IO.Class (liftIO)
import Data.Bits (Bits, (.|.), shiftL, shiftR)
import qualified Data.Digits as Digits

import Data.List (intercalate)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Options = Options
  { optExpression :: String
  }

type Blob = B8.ByteString
type Transform = Conduit Blob IO Blob

-- | Split on a delimeter and trim whitespace on each term
split :: String -> String -> [String]
split x = map (unwords . words) . Split.splitOn x

-- | Evaluate a function expression
evaluate :: String -> IO ()
evaluate = flip ($$) stdout . foldl (=$=) stdin . map segment . split "->"

segment :: String -> Transform
segment s = case split " " s of
  []     -> CL.map id
  x : xs -> uncurry apply $ opFix x xs

-- Allow no whitespace between operator and operand
opFix :: String -> [String] -> (String, [String])
opFix x@(h:t) xs =
    if elem h "+-*/%^"
    then ([h], if length t > 0 then t:xs else xs)
    else (x, xs)

apply :: String -> [String] -> Conduit B8.ByteString IO B8.ByteString
apply fname args = CL.concatMap (:bArgs) =$= primitive fname
  where bArgs = map (unroll . toInteger . read) args

roll :: B8.ByteString -> Integer
roll = B.foldl' unstep 0 . B.reverse
  where unstep a b = a `shiftL` 8 .|. fromIntegral b

unroll :: (Integral a, Bits a) => a -> B.ByteString
unroll = B.unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

primitive :: String -> Transform
primitive x = case x of
  "asn1der"   -> (CL.map $ AsnE.decodeASN1' AsnBE.DER) 
             =$= filterRight
             =$= CL.map (B8.pack . show)

  -- List operations
  "len"       -> CL.map $ unroll . toInteger . B8.length
  "reverse"   -> CL.map B8.reverse
  "head"      -> CL.map $ B8.singleton . B8.head
  "tail"      -> CL.map B8.tail
  "init"      -> CL.map B8.init
  "last"      -> CL.map $ B8.singleton . B8.last
  "take"      -> eat 2 $ \[xs, n] -> B8.take (fromInteger $ roll n) xs
  "drop"      -> eat 2 $ \[xs, n] -> B8.drop (fromInteger $ roll n) xs

  -- Stream operations
  "id"        -> CL.map id
  "bytes"     -> CL.concatMap $ map B8.singleton . B8.unpack
  "consume"   -> CL.sequence CL.consume =$= CL.map B8.concat
  "lines"     -> CL.concatMap B8.lines
  "repeat"    -> eat' 2 $ \[x, n] -> take (fromInteger $ roll n) $ repeat x
  "unlines"   -> CL.sequence CL.consume =$= CL.map (B8.init . B8.unlines)
  "words"     -> CL.concatMap B8.words
  "unwords"   -> CL.sequence CL.consume =$= CL.map B8.unwords

  -- Number formatting
  "hex"       -> CL.map Hex.hex
  "unhex"     -> CL.mapM Hex.unhex
  "dec"       -> CL.map $ B8.pack . show . roll
  "undec"     -> CL.map $ unroll . toInteger . read . B8.unpack
  "bin"       -> CL.map $ B.concatMap (B8.pack . printf "%0.8b")
  "unbin"     -> CL.map $ unroll . Digits.unDigits 2 . map (toInteger . read) . map (:[]) . B8.unpack

  -- Math
  "+"         -> eat 2 $ unroll . (\[x, y] -> x + y) . map roll
  "-"         -> eat 2 $ unroll . (\[x, y] -> x - y) . map roll
  "*"         -> eat 2 $ unroll . (\[x, y] -> x * y) . map roll
  "/"         -> eat 2 $ unroll . (\[x, y] -> x `div` y) . map roll
  "%"         -> eat 2 $ unroll . (\[x, y] -> x `mod` y) . map roll
  "^"         -> eat 2 $ unroll . (\[x, y] -> x ^ y) . map roll

  -- Encoding
  "b64e"      -> CL.map B64.encode 
  "b64d"      -> CL.map B64.decodeLenient

  -- Hashes
  "blake2s256"  -> hmap H.Blake2s_256
  "blake2s224"  -> hmap H.Blake2s_224
  "blake2sp256" -> hmap H.Blake2sp_256
  "blake2sp224" -> hmap H.Blake2sp_224
  "blake2b512"  -> hmap H.Blake2b_512
  "blake2bp512" -> hmap H.Blake2bp_512
  "md2"         -> hmap H.MD2
  "md4"         -> hmap H.MD4
  "md5"         -> hmap H.MD5
  "sha1"        -> hmap H.SHA1
  "sha224"      -> hmap H.SHA224
  "sha256"      -> hmap H.SHA256
  "sha384"      -> hmap H.SHA384
  "sha512"      -> hmap H.SHA512
  "sha512t256"  -> hmap H.SHA512t_256
  "sha512t224"  -> hmap H.SHA512t_224
  "sha3512"     -> hmap H.SHA3_512
  "sha3384"     -> hmap H.SHA3_384
  "sha3256"     -> hmap H.SHA3_256
  "sha3224"     -> hmap H.SHA3_224
  "keccak512"   -> hmap H.Keccak_512
  "keccak384"   -> hmap H.Keccak_384
  "keccak256"   -> hmap H.Keccak_256
  "keccak224"   -> hmap H.Keccak_224
  "ripemd160"   -> hmap H.RIPEMD160
  "skein256256" -> hmap H.Skein256_256
  "skein256224" -> hmap H.Skein256_224
  "skein512512" -> hmap H.Skein512_512
  "skein512384" -> hmap H.Skein512_384
  "skein512256" -> hmap H.Skein512_256
  "skein512224" -> hmap H.Skein512_224
  "whirlpool"   -> hmap H.Whirlpool

  _             -> error $ printf "Unknown function: '%s'" x
  where
    eat :: Int -> ([B8.ByteString] -> B8.ByteString) -> Transform
    eat n f = (CL.sequence $ CL.take n) =$= CL.map f
    eat' :: Int -> ([B8.ByteString] -> [B8.ByteString]) -> Transform
    eat' n f = (CL.sequence $ CL.take n) =$= CL.concatMap f

hash x = BA.convert . H.hashWith x
hmap :: H.HashAlgorithm a => a -> Conduit B8.ByteString IO B8.ByteString
hmap   = CL.map . hash

stdin :: Source IO B8.ByteString
stdin = liftIO B8.getContents >>= yield

stdout :: Sink B8.ByteString IO ()
stdout = CL.mapM_ B8.putStr

filterRight :: Show a => Conduit (Either a b) IO b
filterRight = CL.mapM $ \x -> case x of Right x -> return x
                                        Left e  -> fail $ show e

readCliOpts :: IO Options
readCliOpts =
  Opts.execParser $ Opts.info (Opts.helper <*> cliOpts)
    ( Opts.fullDesc
   <> Opts.header   "munt - cryptographic function composition" 
   <> Opts.progDesc "Transform input with cryptographic functions." 
   <> Opts.footerDoc (OAHC.unChunk (OAHC.stringChunk fnDoc)) )
  where 
    cliOpts = Options
      <$> Opts.argument Opts.str
          ( Opts.metavar "EXPR"
         <> Opts.help    "Function expression to use for transformation."
          )
    ind = 10
    fnDoc     = printf "Available functions:\n%s" $ 
                intercalate "" (map (uncurry list) sections)
    list t xs = printf "  %-7s %s\n" t $ drop ind (indentedList ind 80 xs)
    sections  = [("Encode", encodings), ("Format", formats), ("Math", math),
                 ("List", listfns), ("Stream", stream), ("Hash", hashes)]
    encodings = ["b64e", "b64d"]
    formats   = ["bin", "dec", "hex", "unbin", "undec", "unhex"]
    math      = ["+", "-", "*", "/", "%", "^"]
    stream    = ["bytes", "consume", "id", "lines", "repeat", "unlines",
                 "unwords", "words"]
    listfns   = ["drop", "head", "init", "last", "len", "reverse", "tail", 
                 "take"]
    hashes    = ["blake2s256", "blake2s224", "blake2sp256", "blake2sp224",
                 "blake2b512", "blake2bp512", "md2", "md4", "md5", "sha1",
                 "sha224", "sha256", "sha384", "sha512", "sha512t256",
                 "sha512t224", "sha3512", "sha3384", "sha3256", "sha3224",
                 "keccak512", "keccak384", "keccak256", "keccak224",
                 "ripemd160", "skein256256", "skein256224", "skein512512",
                 "skein512384", "skein512256", "skein512224", "whirlpool"]

-- | Display a list of strings indented and wrapped to fit the given width
indentedList :: Int -> Int -> [String] -> String
indentedList indentBy width =
  intercalate "\n" . reverse. foldl addTerm [indent]
  where addTerm (r:rs) x = 
          let r' = printf "%s %s" r x
          in if length r' < width then (r':rs) 
             else addTerm (indent:r:rs) x
        indent = take indentBy $ repeat ' ' 

main :: IO ()
main = readCliOpts >>= \o ->
  let
    expression = optExpression o
  in do
    evaluate expression
    putStrLn ""

