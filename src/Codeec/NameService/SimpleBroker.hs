{-# LANGUAGE OverloadedStrings #-}

module Codeec.NameService.SimpleBroker (
  Frontend(..),
  Backend(..),

  serverJoin,
  clientJoin,
  startBroker,
) where

import System.ZMQ4.Monadic
import Control.Concurrent
import Control.Monad
import Data.ByteString.Char8 (unpack, pack)

newtype Frontend = Frontend { unFE :: String}
newtype Backend  = Backend  { unBE :: String}

startBroker :: Frontend -> Backend -> IO ()
startBroker f b  = runZMQ $ do
  fes <- socket Router
  bind fes $ unFE f
  bes <- socket Dealer
  bind bes $ unBE b
  proxy fes bes Nothing

clientJoin :: Frontend -> IO String
clientJoin f = runZMQ $ do
  requester <- socket Req
  connect requester $ unFE f
  send requester [] "Howdy Server! send your socket info"
  msg <- receive requester
  return $ unpack msg

serverJoin :: Backend -> String -> IO ()
serverJoin b s = void $ forkIO $ runZMQ $ do
  responder <- socket Rep
  connect responder $ unBE b
  forever $ do
    message <- receive responder
    send responder [] $ pack s
