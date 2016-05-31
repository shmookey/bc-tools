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

import qualified Munt.Primitives as Primitives
import qualified Munt.Parsers as Parse
import Munt.Types

pipeline :: Parser Pipeline
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
    functions = let connector     = pipe <|> arrow <|> concatArrow
                    arrow         = const Nothing <$> P.string "->"
                    pipe          = const Nothing <$> P.char '|'
                    concatArrow   = do P.char '|'
                                       n <- (unroll . toInteger) <$> Parse.int
                                       P.char '>'
                                       return . Just $ concatTask n
                    concatTask n  = Task (Primitive "concat") [n] ([1],1)
                    next (Just a) = segment >>= \x -> return [a, x]
                    next Nothing  = segment >>= \x -> return [x]
                in do f1   <- segment
                      fs   <- P.many (Parse.spacePadded connector >>= next)
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
confess :: Show a => String -> a -> Parser a
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
      in (foldl (=$=) input $ map apply fs) $$ output
    Left err -> fail $ show err
    --in liftIO (putStrLn (show sources)) >> liftIO (putStrLn (show fs)) >> (foldl (=$=) input $ map (\(f,a,i) -> apply f a i) fs) $$ output

apply :: Task -> Conduit Blob IO Blob
apply (Task fn args (steps, cycle)) = liftIO conduit >>= \c -> 
  if cycle == 1 then c
  else let skips       = CL.filter fst 
                     =$= CL.map snd
           runs        = CL.filter (not . fst) 
                     =$= CL.map snd 
                     =$= c
           mark        = CL.sequence (CL.take cycle) =$= CL.concatMap (map skip . zip [1..])
           skip (i,x)  = (not $ elem i steps, x)
           both a b    = C.getZipConduit $ C.ZipConduit a <* C.ZipConduit b
       in mark =$= both skips runs
  where conduit = case fn of 
         Primitive x  -> Primitives.primitive x args
         Compound fns -> return $ foldl1 (=$=) (map apply fns)

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


stdin :: Source IO B8.ByteString
stdin = liftIO B8.getContents >>= yield

stdout :: Sink B8.ByteString IO ()
stdout = CL.mapM_ B8.putStr

filterRight :: Show a => Conduit (Either a b) IO b
filterRight = CL.mapM $ \x -> case x of Right x -> return x
                                        Left e  -> fail $ show e

