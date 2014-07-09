{-# LANGUAGE OverloadedStrings #-}

module SimpleBrokerNameService (
  serverJoin,
  clientJoin,
  startNameServer,
) where

import System.ZMQ4.Monadic
import Control.Concurrent
import Control.Monad
import Data.ByteString.Char8 (unpack, pack)

data NameServerInfo = NameServerInfo {frontend :: String, backend :: String}
data ServerInfo = ServerInfo {nameserverSI :: String, mysocket :: String}
data ClientInfo = ClientInfo {nameserverCI :: String}

startNameServer :: NameServerInfo -> IO ()
startNameServer info = runZMQ $ do
  fe <- socket Router
  bind fe $ frontend info
  be <- socket Dealer
  bind be $ backend info
  proxy fe be Nothing

clientJoin :: ClientInfo -> IO String
clientJoin info = runZMQ $ do
  requester <- socket Req
  connect requester $ nameserverCI info
  send requester [] "Howdy Server! send your socket info"
  msg <- receive requester
  return $ unpack msg

serverJoin :: ServerInfo -> IO ()
serverJoin info = void $ forkIO $ runZMQ $ do
  responder <- socket Rep
  connect responder $ nameserverSI info
  forever $ do
    message <- receive responder
    send responder [] $ pack $ mysocket info
