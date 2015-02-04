{-# LANGUAGE OverloadedStrings #-}

module Codeec.NameService.SimpleBroker (
  Frontend(..),
  Backend(..),

  serverJoin,
  clientJoin,
  startBroker,
) where

import qualified System.ZMQ4 as ZMQ4
import System.ZMQ4.Monadic
import Control.Concurrent
import Control.Monad
import Data.ByteString.Char8 (unpack, pack)
import System.Directory
import System.Posix.Process
import Control.Monad.Trans (liftIO)

#define DEBUG

debugPrint :: String -> IO ()
#ifdef DEBUG
debugPrint s = do
  tid <- myThreadId
  putStrLn $ "[" ++ (show tid) ++ "] " ++ s
#else
debugPrint _ = return ()
#endif

newtype Frontend = Frontend { unFE :: String}
newtype Backend  = Backend  { unBE :: String}

startBroker :: Frontend -> Backend -> IO ()
startBroker f b  = runZMQ $ do
  fes <- socket Router
  bind fes $ unFE f
  bes <- socket Dealer
  bind bes $ unBE b
  proxy fes bes Nothing

clientJoin :: Frontend -> IO (String, ZMQ4.Socket ZMQ4.Req)
clientJoin f = do
  serverAddr <- runZMQ $ do
    requester <- socket Req
    liftIO $ debugPrint "clientJoin(1)"
    connect requester $ unFE f
    liftIO $ debugPrint "clientJoin(2)"
    send requester [] "Howdy Server! send your socket info"
    liftIO $ debugPrint "clientJoin(3)"
    msg <- receive requester
    liftIO $ debugPrint "clientJoin(4)"
    return $ unpack msg
  -- Connect to the shim layer node.
  ctxt <- ZMQ4.context
  sock <- ZMQ4.socket ctxt ZMQ4.Req
  ZMQ4.connect sock serverAddr
  return (serverAddr, sock)


serverJoin :: Backend -> Int {- Port# -} -> IO ()
serverJoin b port = do
  {- Fork a daemon thread that joins with the backend. The daemon shares the
   - servers address for every client request. The client then joins with the
   - server.
   -}
  void $ forkIO $ runZMQ $ do
    responder <- socket Rep
    liftIO $ debugPrint "serverJoin(1)"
    connect responder $ unBE b
    liftIO $ debugPrint "serverJoin(2)"
    forever $ do
      message <- receive responder
      liftIO $ debugPrint "serverJoin(3)"
      {- localhost should be a public ip -}
      send responder [] $ pack $ "tcp://localhost:" ++ show port
      liftIO $ debugPrint "serverJoin(4)"

  runZMQ $ do
    {- Create a router and a dealer -}
    routerSock <- socket Router
    let myaddr = "tcp://*:" ++ show port
    bind routerSock myaddr

    dealerSock <- socket Dealer
    liftIO $ createDirectoryIfMissing False "/tmp/quelea"
    pid <- liftIO $ getProcessID
    bind dealerSock $ "ipc:///tmp/quelea/" ++ show pid

    {- Start proxy to distribute requests to workers -}
    proxy routerSock dealerSock Nothing
