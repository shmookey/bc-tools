module Munt.Parsers where

import qualified Text.Parsec as P
import Data.Char (Char)
import Control.Monad (void)
import Data.List (intercalate)
import Options.Applicative ((<|>))
import Data.Functor.Identity

import Munt.Types


-- Files, paths and directories
-- ---------------------------------------------------------------------

-- | Parse an optionally-quoted file path. Consumes nothing on failure.
-- Stops without consuming the given characters if the path is unquoted.
path :: [Char] -> Parser FilePath
path xs = P.try (quotedPath <|> unquotedPath xs)

-- | Parse a quoted file path. Currently can be any string.
quotedPath :: Parser FilePath
quotedPath = quotedString

-- | Parse an unquoted file path. Stops without consuming the given characters.
unquotedPath :: [Char] -> Parser FilePath
unquotedPath xs =
  let segment = do x  <- P.noneOf ('-':invalid)
                   xs <- stringUntil1 invalid
                   return (x:xs)
      invalid = "/ " ++ xs
      parts   = P.sepBy1 segment $ P.char '/'
  in intercalate "/" <$> parts


-- Separated lists
-- ---------------------------------------------------------------------

-- | Parse a comma-separated list. Spaces allowed, no trailing comma.
commaSeparated :: Parser a -> Parser [a]
commaSeparated p = P.sepBy p commaDelimiter

-- | Parse a non-empty comma-separated list. Spaces allowed, no trailing comma.
commaSeparated1 :: Parser a -> Parser [a]
commaSeparated1 p = P.sepBy1 p commaDelimiter

-- | Parse a comma-separated list. No spaces, no trailing comma.
commaSeparated' :: Parser a -> Parser [a]
commaSeparated' p = P.sepBy p commaDelimiter

-- | Parse a non-empty comma-separated list. No spaces, no trailing comma.
commaSeparated1' :: Parser a -> Parser [a]
commaSeparated1' p = P.sepBy1 p commaDelimiter

-- | Parse a comma. Optional leading/trailing space, failure consumes nothing.
commaDelimiter :: Parser ()
commaDelimiter = void . P.try $ P.between P.spaces P.spaces (P.char ',')

-- | Parse a space-separated list.
spaceSeparated :: Parser a -> Parser [a]
spaceSeparated p = P.sepEndBy p P.space

-- | Parse a non-empty space-separated list.
spaceSeparated1 :: Parser a -> Parser [a]
spaceSeparated1 p = P.sepEndBy1 p P.space


-- String literals
-- ---------------------------------------------------------------------

-- | Parse a quoted string.
quotedString :: Parser String
quotedString = doubleQuotedString <|> singleQuotedString

-- | Parse a double-quoted string.
doubleQuotedString :: Parser String
doubleQuotedString = delimitedString '"'

-- | Parse a single-quoted string.
singleQuotedString :: Parser String
singleQuotedString = delimitedString '\''

-- | Parse a string enclosed by a single character delimiter.
delimitedString :: Char -> Parser String
delimitedString c =
  let delim  = P.char c
      string = stringUntil (c:[])
  in P.between delim delim string

-- | Parse a non-empty string until we see a terminating character, but don't parse that.
stringUntil1 :: [Char] -> Parser String
stringUntil1 cs = 
  let valid   = P.noneOf cs
      invalid = void (P.oneOf cs) <|> P.eof
  in do x  <- valid
        xs <- P.manyTill valid (stopOn invalid)
        return (x:xs)

-- | Parse a string until we see a terminating character, but don't parse that.
stringUntil :: [Char] -> Parser String
stringUntil cs = 
  let valid   = P.noneOf cs
      invalid = void (P.oneOf cs) <|> P.eof
  in P.manyTill valid (stopOn invalid)


-- Numeric sequences, ranges and intervals
-- ---------------------------------------------------------------------

-- | Parse a comma-separated list of ints. No spaces.
ints :: Parser [Int]
ints = commaSeparated1' int

-- | Parse interval notation (e.g. "3,4,5/8"). Consumes nothing on failure.
interval :: Parser ([Int], Int)
interval = P.try $ 
  do steps <- ints
     P.char '/'
     period <- int
     return (steps, period)


-- Simple utility parsers
-- ---------------------------------------------------------------------

-- | Parse an integer literal
int :: Parser Int
int = read `fmap` P.many1 P.digit

-- | Consume no input regardless of whether the parser succeeds or fails.
stopOn :: Parser a -> Parser a
stopOn = P.lookAhead . P.try

-- | Parse an item optionally surrounded by spaces. Consumes those spaces.
spacePadded :: Parser a -> Parser a
spacePadded = P.between P.spaces P.spaces

-- | Parse an item surrounded by braces
embrace :: Parser a -> Parser a
embrace = P.between (P.char '{') (P.char '}')

