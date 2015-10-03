{-# LANGUAGE OverloadedStrings #-}

module Quelea.NameService.LoadBalancingBroker (
  mkNameService,
  startBroker
) where

import qualified System.ZMQ4 as ZMQ4
import System.ZMQ4.Monadic
import Control.Concurrent
import Control.Monad
import Data.ByteString.Char8 (unpack, pack)
import System.Directory
import System.Posix.Process
import Control.Monad.Trans (liftIO)
import Quelea.NameService.Types

-- #define DEBUG

debugPrint :: String -> IO ()
#ifdef DEBUG
debugPrint s = do
  tid <- myThreadId
  putStrLn $ "[" ++ (show tid) ++ "] " ++ s
#else
debugPrint _ = return ()
#endif

startBroker :: Frontend -> Backend -> IO ()
startBroker f b  = runZMQ $ do
  fes <- socket Router
  bind fes $ unFE f
  bes <- socket Dealer
  bind bes $ unBE b
  proxy fes bes Nothing

clientJoin :: Frontend -> IO (String, ZMQ4.Socket ZMQ4.Req)
clientJoin f = do
  ctxt <- ZMQ4.context
  sock <- ZMQ4.socket ctxt ZMQ4.Req
  ZMQ4.connect sock serverAddr
  return (serverAddr, sock)
  where
    serverAddr = unFE f

serverJoin :: Backend -> String {- ip -} -> Int {- Port# -} -> IO ()
serverJoin b _ _ = runZMQ $ do
    {- Create a router socket and connect with the backend -}
    routerSock <- socket Router
    connect routerSock $ unBE b

    {- Create a dealer socket, bind it to a local ipc file -}
    dealerSock <- socket Dealer
    liftIO $ createDirectoryIfMissing False "/tmp/quelea"
    pid <- liftIO $ getProcessID
    bind dealerSock $ "ipc:///tmp/quelea/" ++ show pid

    {- Start proxy to distribute requests to workers -}
    proxy routerSock dealerSock Nothing

mkNameService :: Frontend -> Backend
              -> String {- Backend ip (only for sticky) -}
              -> Int {- Backend port (only for sticky) -}
              -> NameService
mkNameService fe be ip port =
  NameService fe (clientJoin fe) (serverJoin be ip port)
