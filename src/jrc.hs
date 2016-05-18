-- jrc -- json-rpc request client

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as B8L
import qualified Data.Streaming.Network as NetStream
import qualified Options.Applicative as Opts
import qualified Network.Socket as Socket
import Options.Applicative ((<>))

import qualified JsonRpc.Request as Request
import qualified JsonRpc.Response as Response

data Options = Options
  { optSocket :: String
  , optMethod :: String
  } deriving (Show)

readCliOpts :: IO Options
readCliOpts =
  Opts.execParser $ Opts.info (Opts.helper <*> cliOpts)
    ( Opts.fullDesc
   <> Opts.header   "jrc -- JSON-RPC request client" 
   <> Opts.progDesc "Query a JSON-RPC server." )
  where 
    cliOpts = Options
      <$> Opts.argument Opts.str
          ( Opts.metavar "SOCKET"
         <> Opts.help    "Path to socket file." )
      <*> Opts.argument Opts.str
          ( Opts.metavar "METHOD"
         <> Opts.help    "Method name to call." )

main :: IO ()
main = readCliOpts >>= \o ->
  let
    socketPath = optSocket o
    method     = optMethod o
    request    = Request.request method
    msgData    = Aeson.encode request
  in do
    sock <- NetStream.getSocketUnix socketPath
    Socket.send sock $ B8L.unpack msgData
    responseData <- Socket.recv sock 4096
    Socket.close sock
    let result = case Aeson.decode (B8L.pack responseData) of
                   Just x -> Response.result x
                   Nothing -> "Error parsing response: " ++ responseData
    putStrLn result

