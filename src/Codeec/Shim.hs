{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls,
    TemplateHaskell, DataKinds, OverloadedStrings,
    DoAndIfThenElse#-}

module Codeec.Shim (
 runShimNode,
 mkDtLib
) where

import Codeec.Types
import Codeec.NameService.SimpleBroker
import Codeec.Marshall
import Codeec.DBDriver
import Codeec.ShimLayer.Cache
import Codeec.Contract.Language

import System.Directory
import System.Posix.Process
import Data.Serialize
import Control.Applicative ((<$>))
import Control.Monad (forever, replicateM)
import Data.ByteString hiding (map, pack, putStrLn)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as M
import System.ZMQ4.Monadic
import qualified System.ZMQ4 as ZMQ
import Data.Maybe (fromJust)
import Control.Lens
import Database.Cassandra.CQL
import Data.UUID
import Data.Int (Int64)
import qualified Data.Set as S
import Data.Text hiding (map)
import Debug.Trace
import Control.Concurrent (forkIO, threadDelay)

makeLenses ''Addr
makeLenses ''DatatypeLibrary
makeLenses ''Request

-- This is the maximum number of outstanding StickyAvailable requrests.
#define NUM_WORKERS 5

runShimNode :: OperationClass a
            => DatatypeLibrary a
            -> [Server] -> Keyspace -- Cassandra connection info
            -> Backend -> Int       -- Shim layer broker connection info
            -> IO ()
runShimNode dtLib serverList keyspace backend port = runZMQ $ do

  {- Join with broker, and send the nodes address as a message. Whenever a new
   - client joins, the broker will share one of the shim layer's address. The
   - client subsequently connects to this shim layer node. -}
  liftIO $ serverJoin backend $ "tcp://localhost:" ++ show port

  {- Create a router and a dealer -}
  routerSock <- socket Router
  let myaddr = "tcp://*:" ++ show port
  bind routerSock myaddr
  dealerSock <- socket Dealer
  liftIO $ createDirectoryIfMissing False "/tmp/quelea"
  pid <- liftIO $ getProcessID
  bind dealerSock $ "ipc:///tmp/quelea/" ++ show pid

  {- Connection to the Cassandra deployment -}
  pool <- liftIO $ newPool serverList keyspace Nothing
  {- Spawn cache manager -}
  cache <- liftIO $ initCacheManager pool
  {- Spawn a pool of workers -}
  replicateM NUM_WORKERS (liftIO $ forkIO $ worker dtLib pool cache)
  {- Start proxy to distribute requests to workers -}
  proxy routerSock dealerSock Nothing

worker :: OperationClass a => DatatypeLibrary a -> Pool -> CacheManager -> IO ()
worker dtLib pool cache = do
  ctxt <- ZMQ.context
  sock <- ZMQ.socket ctxt ZMQ.Rep
  pid <- getProcessID
  ZMQ.connect sock $ "ipc:///tmp/quelea/" ++ show pid

  {- loop forver servicing clients -}
  forever $ do
    binReq <- ZMQ.receive sock
    let req = decodeRequest binReq
    {- Fetch the operation from the datatype library using the object type and
    - operation name. -}
    let (op,av) = fromJust $ dtLib ^. avMap ^.at (req^.objTypeReq, req^.opReq)
    case av of
      High -> doOp op cache req sock
      Un -> processSAOp req op cache sock
  where
    processSAOp req op cache sock =
      if req^.sqnReq == 0
      then doOp op cache req sock
      else do
        res <- doesCacheInclude cache (req^.objTypeReq) (req^.keyReq) (req^.sidReq) (req^.sqnReq)
        if res
        then doOp op cache req sock
        else waitForCacheRefresh cache >> processSAOp req op cache sock


doOp :: OperationClass a => GenOpFun -> CacheManager -> Request a -> ZMQ.Socket ZMQ.Rep -> IO ()
doOp op cache request sock = do
  let (Request objType key operName arg sessid seqno) = request
  -- Fetch the current context
  (ctxt, deps) <- getContext cache objType key
  let (res, effM) = op ctxt arg
  -- Add current location to the ones for which updates will be fetched
  addHotLocation cache objType key
  result <- case effM of
    Nothing -> return $ Response seqno res
    Just eff -> do
      -- Write effect to cache, which writes through to DB
      writeEffect cache objType key (Addr sessid (seqno+1)) eff deps
      return $ Response (seqno + 1) res
  -- Reply with result
  ZMQ.send sock [] $ encode result

mkDtLib :: OperationClass a => [(a, GenOpFun, Availability, Contract a)] -> DatatypeLibrary a
mkDtLib l = DatatypeLibrary $ Prelude.foldl core M.empty l
  where
    core dtlib (op,fun,av,_) = M.insert (getObjType op, op) (fun, av) dtlib
