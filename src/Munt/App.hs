module Munt.App where

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
import qualified Data.List.Split as Split
import qualified System.IO as IO
import qualified Text.Parsec as P

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (Bits, (.|.), (.&.), shiftL, shiftR, xor)
import Data.Conduit (Source, Sink, Conduit, yield, ($$), (=$=))
import Data.Maybe (isJust)
import Options.Applicative ((<|>))
import Text.Printf (printf)

import qualified Munt.Parsers as Parse
import Munt.Parsers (Parser)

data Options = Options
  { optExpression :: String
  , optRunTests   :: Bool
  }

type Blob = B8.ByteString
type Transform = Conduit Blob IO Blob
type Interval  = ([Int], Int)

type Input       = (InputSource, InputMode)
data InputSource = FileSource FilePath | StdIn deriving (Show)
data InputMode   = LineInput | ByteInput | EntireInput deriving (Show)

data Task = Task
  { taskFunction :: Function
  , taskArgs     :: [Blob]
  , taskInterval :: Interval
  }

data Function =
    Primitive String
  | Compound [Task]

data Pipeline = Pipeline
  { inputs :: [Input]
  , tasks  :: [Task]
  }

pipeline :: Parse.Parser Pipeline
pipeline = pipeline
  where
    pipeline = let justInputs    = flip Pipeline [] <$> inputs
                   justFunctions = Pipeline [(StdIn, LineInput)] <$> functions
                   fullPipeline  = do srcs <- Parse.spacePadded inputs
                                      Parse.spacePadded sourceArrow
                                      fns <- functions
                                      return $ Pipeline srcs fns
                   sourceArrow   = void $ P.string "=>"
               in P.try fullPipeline <|> P.try justFunctions

    inputs = let list        = Parse.spaceSeparated input
                 defaults    = return [(StdIn, LineInput)]
                 input       = do m <- mode
                                  s <- source
                                  return (s, m)

                 source      = sourceFile <|> sourceStdin
                 sourceStdin = const StdIn <$> P.string "-"
                 sourceFile  = FileSource <$> Parse.path "="

                 mode        = modeLine <|> modeByte <|> modeDefault
                 modeLine    = const LineInput <$> P.char '^'
                 modeByte    = const ByteInput <$> P.char '*'
                 modeDefault = return EntireInput
             in
               list <|> defaults

    functions :: Parser [Task]
    functions = let arrow         = basicArrow <|> concatArrow
                    basicArrow    = const Nothing <$> P.string "->"
                    concatArrow   = do P.char '|'
                                       n <- (unroll . toInteger) <$> Parse.int
                                       P.char '>'
                                       return . Just $ concatTask n
                    concatTask n  = Task (Primitive "concat") [n] ([1],1)
                    next (Just a) = segment >>= \x -> return [a, x]
                    next Nothing  = segment >>= \x -> return [x]
                in do f1   <- segment
                      fs   <- P.many (Parse.spacePadded arrow >>= next)
                      return (f1 : concat fs)

    segment :: Parser Task
    segment = let name       = P.many1 P.alphaNum
                  operator   = (:[]) <$> P.oneOf "+-*/%^"
                  interval   = Parse.interval <* P.spaces
                  primitive  = Primitive <$> (operator <|> name)
                  arguments  = let arg     = argInt <|> argStr
                                   argInt  = (unroll . toInteger) <$> Parse.int
                                   argStr  = B8.pack <$> Parse.quotedString
                               in P.sepEndBy arg P.space

              in do cycle      <- P.option ([1],1) interval
                    fn         <- Parse.spacePadded $ subexpression <|> primitive
                    args       <- P.option [] $ Parse.spacePadded arguments
                    return $ Task fn args cycle

    subexpression :: Parser Function
    subexpression = Compound <$> Parse.embrace (Parse.spacePadded functions)

-- | Print a debug message from inside a parser
confess :: Show a => String -> a -> Parse.Parser a
confess msg x = liftIO (putStrLn $ msg ++ (show x)) >> return x

-- | Split on a delimeter and trim whitespace on each term
split :: String -> String -> [String]
split x = map (unwords . words) . Split.splitOn x

-- | Evaluate a function expression
evaluate :: String -> IO.Handle -> IO.Handle -> IO ()
evaluate x hIn hOut = 
  P.runParserT pipeline () "(command)" x
  >>= \r -> case r of
    Right (Pipeline sources fs) ->
      let input  = createProducer hIn sources
          output = CC.sinkHandle  hOut
      in (foldl (=$=) input $ map applyTask fs) $$ output
    Left err -> fail $ show err
    --in liftIO (putStrLn (show sources)) >> liftIO (putStrLn (show fs)) >> (foldl (=$=) input $ map (\(f,a,i) -> apply f a i) fs) $$ output

applyTask :: Task -> Conduit Blob IO Blob
applyTask (Task fn args (steps, cycle)) = 
  if cycle == 1 then conduit
  else let skips       = CL.filter fst 
                     =$= CL.map snd
           runs        = CL.filter (not . fst) 
                     =$= CL.map snd 
                     =$= conduit
           mark        = CL.sequence (CL.take cycle) =$= CL.concatMap (map skip . zip [1..])
           skip (i,x)  = (not $ elem i steps, x)
           both a b    = C.getZipConduit $ C.ZipConduit a <* C.ZipConduit b
       in mark =$= both skips runs
  where conduit = case fn of 
         Primitive x  -> primitive x args
         Compound fns -> foldl1 (=$=) $ map applyTask fns

createProducer :: IO.Handle -> [Input] -> Source IO Blob
createProducer stdin srcs = C.sequenceSources (map getSource srcs) =$= CL.concat
  where 
    getSource (FileSource path, inputMode) =
      let getChunk  = liftIO . nextChunk inputMode
          getHandle = liftIO $ unbufferedFileHandle path
          while c t = CC.repeatWhileM t c =$= CL.catMaybes
      in do handle <- getHandle
            while isJust $ getChunk handle
   
    getSource (StdIn, inputMode) =
      liftIO (B.hGetContents stdin) >>= yield

    unbufferedFileHandle :: FilePath -> IO IO.Handle
    unbufferedFileHandle path = 
      do handle <- IO.openBinaryFile path IO.ReadMode
         IO.hSetBuffering handle IO.NoBuffering
         return handle
  
    nextChunk :: InputMode -> IO.Handle -> IO (Maybe Blob)
    nextChunk mode handle = do
      done <- isFinished handle
      if done then return Nothing
      else readChunk mode handle
       >>= return . Just

    isFinished :: IO.Handle -> IO Bool
    isFinished handle = do
      closed <- IO.hIsClosed handle
      if closed then return True
      else do
        eof <- IO.hIsEOF handle
        if eof then return True
               else return False
      

    readChunk :: InputMode -> IO.Handle -> IO Blob
    readChunk LineInput   = B.hGetLine
    readChunk ByteInput   = flip B.hGet 1
    readChunk EntireInput = B.hGetContents 

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
  "take"      -> eat 2 $ \[n, xs]  -> B8.take (fromInteger $ roll n) xs
  "drop"      -> eat 2 $ \[n, xs]  -> B8.drop (fromInteger $ roll n) xs
  "prepend"   -> eat 2 $ \[xs, ys] -> B8.concat [xs, ys]
  "append"    -> eat 2 $ \[xs, ys] -> B8.concat [ys, xs]

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

