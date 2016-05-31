module Munt.Types where

import qualified Data.ByteString.Char8 as B8
import qualified Text.Parsec as P
import Data.Conduit (Conduit)

-- | CLI options and flags
data Options = Options
  { optExpression :: String
  , optRunTests   :: Bool
  }

-- | A chunk of data in the stream.
type Blob = B8.ByteString

-- | A stream transformation process.
type Transform = Conduit Blob IO Blob

-- | Specifies which values from 1 to `n` (inclusive) in a cycle to perform an operation
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

-- | A function which encodes a blob into an `a`
type Cipher a = Blob -> a

-- | Wrapper for Parsec string parser in IO.
type Parser a = P.ParsecT String () IO a

