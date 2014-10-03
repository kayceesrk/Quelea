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
import Codeec.ShimLayer.GC
import Codeec.Contract.Language

import System.Directory
import System.Posix.Process
import Data.Serialize
import Control.Applicative ((<$>))
import Control.Monad (forever, replicateM, when)
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
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Data.Tuple.Select

makeLenses ''Addr
makeLenses ''DatatypeLibrary
makeLenses ''OperationPayload

-- This is the maximum number of outstanding StickyAvailable requrests.
#define NUM_WORKERS 5

-- #define DEBUG

debugPrint :: String -> IO ()
#ifdef DEBUG
debugPrint s = do
  tid <- myThreadId
  putStrLn $ "[" ++ (show tid) ++ "] " ++ s
#else
debugPrint _ = return ()
#endif

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
    txns <- includedTxns cache
    case decodeOperationPayload binReq of
      ReqOper req -> do
        {- Fetch the operation from the datatype library using the object type and
        - operation name. -}
        let (op,av) = fromJust $ dtLib ^. avMap ^.at (req^.objTypeReq, req^.opReq)
        debugPrint $ "worker: before " ++ show (req^.objTypeReq, req^.opReq, av)
        (result, ctxtSize) <- case av of
          High -> doOp op cache req ONE
          Sticky -> processStickyOp req op cache
          Un -> processUnOp req op cache pool
        ZMQ.send sock [] $ encode result
        debugPrint $ "worker: after " ++ show (req^.objTypeReq, req^.opReq)
        -- Maybe perform summarization
        let gcFun = fromJust $ dtLib ^. sumMap ^.at (req^.objTypeReq)
        maybeGCCache cache (req^.objTypeReq) (req^.keyReq) ctxtSize gcFun
      ReqTxnCommit txid deps -> do
        debugPrint $ "Committing transaction " ++ show txid
        when (S.size deps > 0) $ runCas pool $ insertTxn txid deps
        ZMQ.send sock [] $ encode ResCommit
      ReqSnapshot objs -> do
        fetchUpdates cache ALL $ S.toList objs
        snapshot <- snapshotCache cache
        let filteredSnapshot = M.foldlWithKey (\m k v ->
              if S.member k objs then M.insert k v m else m) M.empty snapshot
        ZMQ.send sock [] $ encode $ ResSnapshot filteredSnapshot
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
      debugPrint $ "processUnOp: obtained lock"
      -- Read latest values at the key - under ALL
      fetchUpdates cache ALL [(ot,k)]
      -- Perform the op
      res <- doOp op cache req ALL
      -- Release Lock
      releaseLock ot k sid pool
      return res

doOp :: OperationClass a => GenOpFun -> CacheManager -> OperationPayload a -> Consistency -> IO (Response, Int)
doOp op cache request const = do
  let (OperationPayload objType key operName arg sessid seqno mbtxid) = request
  -- Build the context
  (ctxt, deps) <- buildContext objType key mbtxid
  -- Perform the operation on this context
  let (res, effM) = op ctxt arg
  -- Add current location to the ones for which updates will be fetched
  addHotLocation cache objType key
  result <- case effM of
    Nothing -> return $ ResOper seqno res Nothing Nothing
    Just eff -> do
      -- Write effect writes to DB, and potentially to cache
      writeEffect cache objType key (Addr sessid (seqno+1)) eff deps const $ sel1 <$> mbtxid
      case mbtxid of
        Nothing -> return $ ResOper (seqno + 1) res Nothing Nothing
        Just (_,MAV _ _) -> do
          txns <- getInclTxnsAt cache objType key
          return $ ResOper (seqno + 1) res (Just eff) (Just txns)
        otherwise -> return $ ResOper (seqno + 1) res (Just eff) Nothing
  -- return response
  return (result, Prelude.length ctxt)
  where
    buildContext ot k Nothing = getContext cache ot k
    buildContext ot k (Just (_, RC l)) = do
      (ctxtVanilla, depsVanilla) <- buildContext ot k Nothing
      let (el, as) = S.foldl (\(el,as) (addr, eff) ->
                      (eff:el, S.insert addr as)) ([],S.empty) l
      return (el ++ ctxtVanilla, S.union as depsVanilla)
    buildContext ot k (Just (txid, MAV l txndeps)) = do
      res <- doesCacheIncludeTxns cache txndeps
      if res then buildContext ot k (Just (txid,RC l))
      else do
        fetchTxns cache txndeps
        buildContext ot k (Just (txid, MAV l txndeps))
    buildContext ot k (Just (_,PSI effSet)) = return $
      S.foldl (\(el,as) (addr, eff) -> (eff:el, S.insert addr as))
              ([], S.empty) effSet
    buildContext ot k (Just (_,SER)) = error "Serializability not implemented"


mkDtLib :: OperationClass a => [(a, (GenOpFun, GenSumFun), Availability)] -> DatatypeLibrary a
mkDtLib l =
  let (m1, m2) = Prelude.foldl core (M.empty, M.empty) l
  in DatatypeLibrary m1 m2
  where
    core (m1, m2) (op, (fun1, fun2), av) =
      (M.insert (getObjType op, op) (fun1, av) m1,
       M.insert (getObjType op) fun2 m2)
