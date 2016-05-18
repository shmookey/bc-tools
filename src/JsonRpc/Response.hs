{-# LANGUAGE DeriveGeneric #-}

module JsonRpc.Response
  ( Response(result)
  ) where

import GHC.Generics
import Data.Aeson (FromJSON)


data Response = Response
  { jsonrpc :: String
  , id      :: String
  , result  :: String
  } deriving (Generic)

instance FromJSON Response

