-- arc -- asynchronous request client

import qualified Data.Streaming.Network as NetStream
import qualified Options.Applicative as Opts
import qualified Network.Socket as Socket
import Options.Applicative ((<>))

data Options = Options
  { optSocket :: String
  , optData   :: String
  } deriving (Show)

readCliOpts :: IO Options
readCliOpts =
  Opts.execParser $ Opts.info (Opts.helper <*> cliOpts)
    ( Opts.fullDesc
   <> Opts.header   "arc -- asynchronous request client" 
   <> Opts.progDesc "Query an asynchronously-responding server." )
  where 
    cliOpts = Options
      <$> Opts.argument Opts.str
          ( Opts.metavar "SOCKET"
         <> Opts.help    "Path to socket file." )
      <*> Opts.argument Opts.str
          ( Opts.metavar "DATA"
         <> Opts.help    "Data to send." )

main :: IO ()
main = readCliOpts >>= \o ->
  let
    socketPath = optSocket o
    msgData    = optData   o
  in do
    sock <- NetStream.getSocketUnix socketPath
    Socket.send sock msgData
    reply <- Socket.recv sock 4096
    Socket.close sock
    putStr reply

