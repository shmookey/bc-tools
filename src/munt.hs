{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- munt - cryptographic function composition

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
import qualified Data.ASN1.Types as AsnT
import qualified Data.ASN1.Stream as AsnS
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
import qualified Data.List.Split as Split
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Chunk as OAHC
import qualified System.IO as IO
import qualified System.Process as Proc
import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec as PC

import Options.Applicative ((<>),(<|>))
import Data.Conduit (Source, Sink, Conduit, await, yield, ($$), (=$=))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (join)
import Data.Bits (Bits, (.|.), (.&.), shiftL, shiftR, xor)
import Data.List (intercalate)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Options = Options
  { optExpression :: String
  }

type Blob = B8.ByteString
type Transform = Conduit Blob IO Blob
type Interval  = ([Int], Int)
data InputSource = FileSource FilePath | StdIn

data Pipeline = Pipeline
  { inputs :: [InputSource]
  , tasks  :: [(String, [Blob], Interval)]
  }

pipeline :: PC.CharParser () Pipeline
pipeline = chain
  where
    fnCall  = let fnName   = operator <|> name
                  name     = P.many1 P.alphaNum
                  operator = (:[]) `fmap` P.oneOf "+-*/%^"
              in do ivl  <- spacePadded interval
                    fn   <- spacePadded fnName
                    args <- spacePadded argList
                    return (fn, args, ivl)

    argList = let arg     = argInt <|> argStr
                  argInt  = (unroll . toInteger) `fmap` intLiteral
                  argStr  = B8.pack `fmap` quotedString
              in P.option [] $ P.sepEndBy arg P.space

    interval = let stepList = P.sepBy intLiteral comma
                   stepDef  = do steps <- stepList
                                 fslash
                                 cycle <- intLiteral
                                 return (steps, cycle)
               in P.option stdInterval stepDef
   
    chain    = let arrow         = spacePadded basicArrow <|> spacePadded concatArrow
                   basicArrow    = (P.string "->") >> return Nothing
                   concatArrow   = do P.char '|'
                                      n <- (unroll . toInteger) `fmap` intLiteral
                                      P.char '>'
                                      return $ Just ("concat", [n], stdInterval)
                   next (Just a) = fnCall >>= \x -> return [a, x]
                   next Nothing  = fnCall >>= \x -> return [x]
               in do srcs <- inputs
                     f1   <- fnCall
                     fs   <- P.many (arrow >>= next)
                     return $ Pipeline srcs (f1 : concat fs)

    inputs = let inputArrow   = (P.string "=>") >> return ()
                 inputSources = P.sepBy (spacePadded inputSource) comma
                 srcSection   = do xs <- spacePadded inputSources
                                   spacePadded inputArrow
                                   return xs
             in P.try srcSection <|> return [StdIn]

    inputSource = let filepath = intercalate "/" `fmap` P.sepEndBy filename fslash
                      filename = rawFile <|> quotedString
                      rawFile  = do x  <- P.noneOf "-/, ="
                                    xs <- P.many $ P.noneOf ("/, =")
                                    return (x:xs)
                  in (P.string "-" >> return StdIn)
                 <|> (filepath >>= return . FileSource)
    
    stdInterval  = ([1], 1)
    quotedString = P.between dblQuote dblQuote $ P.many1 nonDblQuote
    intLiteral   = read `fmap` P.many1 P.digit
    spacePadded  = P.between P.spaces P.spaces
    dblQuote     = P.char '"'
    nonDblQuote  = P.noneOf "\""
    comma        = P.char ','
    fslash       = P.char '/'

-- | Split on a delimeter and trim whitespace on each term
split :: String -> String -> [String]
split x = map (unwords . words) . Split.splitOn x

-- | Evaluate a function expression
evaluate :: String -> IO.Handle -> IO.Handle -> IO ()
evaluate x hIn hOut = case P.parse pipeline "(command)" x of
  Left err -> fail $ show err
  Right (Pipeline sources fs) ->
    let input  = createProducer hIn sources
        output = CC.sinkHandle  hOut
    in (foldl (=$=) input $ map (\(f,a,i) -> apply f a i) fs) $$ output

createProducer :: IO.Handle -> [InputSource] -> Source IO Blob
createProducer stdin = foldl1 (>>) . map getSource
  where getSource (FileSource p) = liftIO (B.readFile p) >>= yield
        getSource StdIn          = liftIO (B.hGetContents stdin) >>= yield

apply :: String -> [Blob] -> Interval -> Conduit Blob IO Blob
apply fname args (steps, cycle) =
  if cycle == 1 then primitive fname args
  else let skips       = CL.filter fst 
                     =$= CL.map snd
           runs        = CL.filter (not . fst) 
                     =$= CL.map snd 
                     =$= primitive fname args
           mark        = CL.sequence (CL.take cycle) =$= CL.concatMap (map skip . zip [1..])
           skip (i,x)  = (not $ elem i steps, x)
           both a b    = C.getZipConduit $ C.ZipConduit a <* C.ZipConduit b
       in mark =$= both skips runs

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

type Cipher a = Blob -> a

primitive :: String -> [Blob] -> Transform
primitive x args = case x of
  "trace"    -> CC.iterM (putStrLn . printf "TRACE: %s" . show)
  "pasn1d"   -> (CL.map $ AsnE.decodeASN1' AsnBE.DER) 
            =$= filterRight
            =$= CL.concatMap (map (B8.pack . show))

  -- List operations
  "len"       -> CL.map $ unroll . toInteger . B8.length
  "reverse"   -> CL.map B8.reverse
  "head"      -> CL.map $ B8.singleton . B8.head
  "tail"      -> CL.map B8.tail
  "init"      -> CL.map B8.init
  "last"      -> CL.map $ B8.singleton . B8.last
  "take"      -> eat 2 $ \[n, xs] -> B8.take (fromInteger $ roll n) xs
  "drop"      -> eat 2 $ \[n, xs] -> B8.drop (fromInteger $ roll n) xs

  -- Stream operations
  "id"        -> CL.map id
  "after"     -> CL.concatMap (:args)
  "before"    -> CL.concatMap (\x -> args ++ [x])
  "bytes"     -> CL.concatMap $ map B8.singleton . B8.unpack
  "consume"   -> CL.sequence CL.consume =$= CL.map B8.concat
  "count"     -> CL.sequence CL.consume =$= CL.map (unroll . length)
  "lines"     -> CL.concatMap B8.lines
  "repeat"    -> eat' 2 $ \[n, x] -> take (fromInteger $ roll n) $ repeat x
  "filter"    -> CL.filter (const False)
  "flip"      -> eat' 2 $ \[x, y] -> [y, x]
  "unlines"   -> CL.sequence CL.consume =$= CL.map (B8.init . B8.unlines)
  "words"     -> CL.concatMap B8.words
  "unwords"   -> CL.sequence CL.consume =$= CL.map B8.unwords
  "concat"    -> case args of 
                    [n] -> CL.sequence (stake n) =$= sconcat
                    _   -> CL.sequence (CL.take 1 >>= stake . head) =$= sconcat

  -- Number formatting
  "hex"       -> CL.map Hex.hex
  "unhex"     -> CL.mapM Hex.unhex
  "dec"       -> CL.map $ B8.pack . show . roll
  "undec"     -> CL.map $ unroll . toInteger . read . B8.unpack
  "bin"       -> CL.map $ B.concatMap (B8.pack . printf "%0.8b")
  "unbin"     -> CL.map $ unroll . Digits.unDigits 2 . map (toInteger . read) . map (:[]) . B8.unpack

  -- Math
  "+"         -> eat 2 $ unroll . (\[d, x] -> x + d)     . map roll
  "-"         -> eat 2 $ unroll . (\[d, x] -> x - d)     . map roll
  "*"         -> eat 2 $ unroll . (\[d, x] -> x * d)     . map roll
  "^"         -> eat 2 $ unroll . (\[d, x] -> x ^ d)     . map roll
  "/"         -> eat 2 $ unroll . (\[d, x] -> x `div` d) . map roll
  "%"         -> eat 2 $ unroll . (\[d, x] -> x `mod` d) . map roll

  -- Bitwise
  "and"       -> eat 2 $ unroll . (\[x, y] -> x .&. y)   . map roll
  "or"        -> eat 2 $ unroll . (\[x, y] -> x .|. y)   . map roll
  "xor"       -> eat 2 $ unroll . (\[x, y] -> x `xor` y) . map roll
  "not"       -> CL.map $ unroll . Bits.complement . roll
  "rsh"       -> eat 2 $ unroll . (\[n, x] -> x `shiftR` (fromInteger n)) . map roll
  "lsh"       -> eat 2 $ unroll . (\[n, x] -> x `shiftL` (fromInteger n)) . map roll

  -- Encoding
  "b64e"      -> CL.map B64.encode 
  "b64d"      -> CL.map B64.decodeLenient

  -- Cipher
  "aes128e"   -> ecbEncrypt (cipher :: Cipher AES.AES128)
  "aes128d"   -> ecbDecrypt (cipher :: Cipher AES.AES128)
  "aes192e"   -> ecbEncrypt (cipher :: Cipher AES.AES192)
  "aes192d"   -> ecbDecrypt (cipher :: Cipher AES.AES192)
  "aes256e"   -> ecbEncrypt (cipher :: Cipher AES.AES256)
  "aes256d"   -> ecbDecrypt (cipher :: Cipher AES.AES256)
  "cam128e"   -> ecbEncrypt (cipher :: Cipher Camellia.Camellia128) 
  "cam128d"   -> ecbDecrypt (cipher :: Cipher Camellia.Camellia128) 
  "dese"      -> ecbEncrypt (cipher :: Cipher DES.DES) 
  "desd"      -> ecbDecrypt (cipher :: Cipher DES.DES) 
  "deseee3e"  -> ecbEncrypt (cipher :: Cipher TripleDES.DES_EEE3) 
  "deseee3d"  -> ecbDecrypt (cipher :: Cipher TripleDES.DES_EEE3) 
  "desede3e"  -> ecbEncrypt (cipher :: Cipher TripleDES.DES_EDE3) 
  "desede3d"  -> ecbDecrypt (cipher :: Cipher TripleDES.DES_EDE3) 
  "deseee2e"  -> ecbEncrypt (cipher :: Cipher TripleDES.DES_EEE2) 
  "deseee2d"  -> ecbDecrypt (cipher :: Cipher TripleDES.DES_EEE2) 
  "desede2e"  -> ecbEncrypt (cipher :: Cipher TripleDES.DES_EDE2) 
  "desede2d"  -> ecbDecrypt (cipher :: Cipher TripleDES.DES_EDE2) 
  "bfe"       -> ecbEncrypt (cipher :: Cipher Blowfish.Blowfish) 
  "bfd"       -> ecbDecrypt (cipher :: Cipher Blowfish.Blowfish) 
  "bf64e"     -> ecbEncrypt (cipher :: Cipher Blowfish.Blowfish64) 
  "bf64d"     -> ecbDecrypt (cipher :: Cipher Blowfish.Blowfish64) 
  "bf128e"    -> ecbEncrypt (cipher :: Cipher Blowfish.Blowfish128) 
  "bf128d"    -> ecbDecrypt (cipher :: Cipher Blowfish.Blowfish128) 
  "bf256e"    -> ecbEncrypt (cipher :: Cipher Blowfish.Blowfish256) 
  "bf256d"    -> ecbDecrypt (cipher :: Cipher Blowfish.Blowfish256) 
  "bf448e"    -> ecbEncrypt (cipher :: Cipher Blowfish.Blowfish448) 
  "bf448d"    -> ecbDecrypt (cipher :: Cipher Blowfish.Blowfish448)

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
    eat :: Int -> ([Blob] -> Blob) -> Transform
    eat n f = let pre = take n args
                  rem = n - (length args)
               in (CL.sequence $ CL.take rem) =$= CL.map (\xs -> f (pre ++ xs))
    eat' :: Int -> ([B8.ByteString] -> [B8.ByteString]) -> Transform
    eat' n f = let pre = take n args
                   rem = n - (length args)
               in (CL.sequence $ CL.take rem) =$= CL.concatMap (\xs -> f (pre ++ xs))
    cipher :: Cipher.Cipher a => Blob -> a
    cipher k = CError.throwCryptoError $ Cipher.cipherInit k
    some n   = CL.sequence $ CL.take n
    sconcat  = CL.map B.concat
    stake    = CL.take . roll'
    ecbEncrypt f = eat 2 $ \[k, c] -> Cipher.ecbEncrypt (f k) c
    ecbDecrypt f = eat 2 $ \[k, c] -> Cipher.ecbDecrypt (f k) c

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
          ( Opts.metavar "[ sources => ] action [ -> action -> ... ]"
         <> Opts.help    "Function expression to use for transformation."
          )
    ind       = 10
    fnDoc     = printf "Available functions:\n%s" $ intercalate "" sections
    list t xs = printf "  %-7s %s\n" t $ drop ind (indentedList ind 80 xs)
    sections  = map (uncurry list)
      [ ("Encode", ["b64e", "b64d"])
      , ("Format", ["bin", "dec", "hex", "unbin", "undec", "unhex"])
      , ("Math",   ["+", "-", "*", "/", "%", "^"])
      , ("Bitwise",["and", "or", "xor", "not", "rsh", "lsh"])
      , ("Util",   ["id", "trace"])
      , ("List",   ["drop", "head", "init", "last", "len", "reverse", "tail",
                    "take"])
      , ("Stream", ["after", "before", "bytes", "concat", "consume", "count", 
                    "filter", "flip", "lines", "repeat", "unlines", "unwords",
                    "words"])
      , ("Cipher", ["aes128d", "aes128e", "aes192d", "aes192e", "aes256d", 
                    "aes256e", "bfe", "bfd", "bf64e", "bf64d", "bf128e", 
                    "bf128d", "bf256e", "bf256d", "bf448e", "bf448d", "dese", 
                    "desd", "deseee3e", "deseee3d", "desede3e", "desede3d", 
                    "deseee2e", "deseee2d", "desede2e", "desede2d", "cam128e",
                    "cam128d"])
      , ("Hash",   ["blake2s256", "blake2s224", "blake2sp256", "blake2sp224",
                    "blake2b512", "blake2bp512", "md2", "md4", "md5", "sha1",
                    "sha224", "sha256", "sha384", "sha512", "sha512t256",
                    "sha512t224", "sha3512", "sha3384", "sha3256", "sha3224",
                    "keccak512", "keccak384", "keccak256", "keccak224",
                    "ripemd160", "skein256256", "skein256224", "skein512512",
                    "skein512384", "skein512256", "skein512224", "whirlpool"])
      ]

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
    --runTests
    evaluate expression IO.stdin IO.stdout
    putStrLn ""

runTests :: IO ()
runTests =
  do
    putStrLn "Test results: "
    test "reverse"                  "123456" "654321"
    test "take 3"                   "123456" "123"
    test "+1"                       "ABC"    "BBC"
    test "-1"                       "234"    "134"
    test "bytes -> + 1"             "ABC"    "BCD"
    test "dec"                      "A"      "65"
    test "undec"                    "65"     "A"
    test "bytes -> concat 2 -> + 1" "1234"   "2244"
    test "bytes |2> +1"             "1234"   "2244"
    test "bytes -> 1/4 filter"      "12345"  "234"
  where
    test expr input expected =
      do result <- B8.unpack `fmap` runExpr expr (B8.pack input)
         if result == expected
         then putStrLn $ "pass -- " ++ repr
         else putStrLn $ "FAIL -- " ++ repr ++ " (got: " ++ result ++ ")"
      where repr = printf "%s => [ %s ] => %s" input expr expected
    runExpr expr input = 
      do (rIn, wIn)   <- Proc.createPipe
         (rOut, wOut) <- Proc.createPipe
         IO.hSetBuffering rIn IO.NoBuffering
         IO.hSetBuffering rOut IO.NoBuffering
         IO.hSetBuffering wIn IO.NoBuffering
         IO.hSetBuffering wOut IO.NoBuffering
         B.hPut wIn input
         IO.hFlush wIn
         IO.hClose wIn
         evaluate expr rIn wOut
         output <- B.hGetSome rOut 128
         IO.hClose rIn
         IO.hClose rOut
         IO.hClose wOut
         return output

