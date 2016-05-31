-- munt - cryptographic function composition

import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Chunk as OAHC
import qualified System.IO as IO
import qualified System.Process as Proc

import Data.List (intercalate)
import Options.Applicative ((<>))
import Text.Printf (printf)

import Munt.Types
import qualified Munt.App as App


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
         <> Opts.value   ""
         <> Opts.help    "Function expression to use for transformation."
          )
     <*> Opts.switch
         ( Opts.long  "test"
        <> Opts.short 't'
        <> Opts.help  "Run tests." )
    ind       = 10
    fnDoc     = printf "Available functions:\n%s" $ intercalate "" sections
    list t xs = printf "  %-7s %s\n" t $ drop ind (indentedList ind 80 xs)
    sections  = map (uncurry list)
      [ ("Encode", ["b64e", "b64d"])
      , ("Format", ["bin", "dec", "hex", "unbin", "undec", "unhex"])
      , ("Math",   ["+", "-", "*", "/", "%", "^"])
      , ("Bitwise",["and", "or", "xor", "not", "rsh", "lsh"])
      , ("Util",   ["id", "trace"])
      , ("List",   ["append", "drop", "head", "init", "last", "len", "prepend",
                    "reverse", "tail", "take"])
      , ("Stream", ["after", "before", "bytes", "concat", "consume", "count", 
                    "dup", "filter", "flip", "lines", "repeat", "unlines", 
                    "unwords", "words"])
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
    testMode   = optRunTests o
  in do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetBuffering IO.stdout IO.NoBuffering
    App.evaluate expression IO.stdin IO.stdout
    putStrLn ""

