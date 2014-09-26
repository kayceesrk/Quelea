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
makeLenses ''OperationPayload

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
    let req = decodeOperationPayload binReq
    {- Fetch the operation from the datatype library using the object type and
    - operation name. -}
    let (op,av) = fromJust $ dtLib ^. avMap ^.at (req^.objTypeReq, req^.opReq)
    (result, ctxtSize) <- case av of
      High -> doOp op cache req ONE
      Sticky -> processStickyOp req op cache
      Un -> processUnOp req op cache pool
    ZMQ.send sock [] $ encode result
    -- Maybe perform summarization
    let gcFun = fromJust $ dtLib ^. sumMap ^.at (req^.objTypeReq)
    maybeGCCache cache (req^.objTypeReq) (req^.keyReq) ctxtSize gcFun
  where
    processStickyOp req op cache =
      -- Check whether this is the first effect in the session <= previous
      -- sequence number is 0.
      if req^.sqnReq == 0
      then doOp op cache req ONE
      else do
        let ot = req^.objTypeReq
        let k = req^.keyReq
        -- Check whether the current cache includes the previous effect
        res <- doesCacheInclude cache ot k (req^.sidReq) (req^.sqnReq)
        if res
        then doOp op cache req ONE
        else do
          -- Read DB, and check cache for previous effect
          fetchUpdates cache ONE [(ot,k)]
          res <- doesCacheInclude cache ot k (req^.sidReq) (req^.sqnReq)
          if res
          then doOp op cache req ONE
          else do
            -- Wait till next cache refresh and repeat the process again
            waitForCacheRefresh cache ot k
            processStickyOp req op cache
    processUnOp req op cache pool = do
      let (ot, k, sid) = (req^.objTypeReq, req^.keyReq, req^.sidReq)
      -- Get Lock
      getLock ot k sid pool
      -- Read latest values at the key - under ALL
      fetchUpdates cache ALL [(ot,k)]
      -- Perform the op
      res <- doOp op cache req ALL
      -- Release Lock
      releaseLock ot k sid pool
      return res

doOp :: OperationClass a => GenOpFun -> CacheManager -> OperationPayload a -> Consistency -> IO (Response, Int)
doOp op cache request const = do
  let (OperationPayload objType key operName arg sessid seqno) = request
  -- Fetch the current context
  (ctxt, deps) <- getContext cache objType key
  let (res, effM) = op ctxt arg
  -- Add current location to the ones for which updates will be fetched
  addHotLocation cache objType key
  result <- case effM of
    Nothing -> return $ Response seqno res
    Just eff -> do
      -- Write effect writes to DB, and potentially to cache
      writeEffect cache objType key (Addr sessid (seqno+1)) eff deps const
      return $ Response (seqno + 1) res
  -- return response
  return (result, Prelude.length ctxt)

mkDtLib :: OperationClass a => [(a, (GenOpFun, GenSumFun), Availability)] -> DatatypeLibrary a
mkDtLib l =
  let (m1, m2) = Prelude.foldl core (M.empty, M.empty) l
  in DatatypeLibrary m1 m2
  where
    core (m1, m2) (op, (fun1, fun2), av) =
      (M.insert (getObjType op, op) (fun1, av) m1,
       M.insert (getObjType op) fun2 m2)
