{-# LANGUAGE DeriveGeneric #-}

module JsonRpc.Request
  ( Request
  , request
  ) where

import GHC.Generics
import Data.Aeson (ToJSON)

data Request = Request
  { jsonrpc :: String
  , method  :: String
  , params  :: [String]
  , id      :: String
  } deriving (Generic)

instance ToJSON Request


defaultRequest :: Request
defaultRequest = Request "2.0" "" [] "1"

request :: String -> Request
request x = defaultRequest { method = x }

