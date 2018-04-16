{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls,
    TemplateHaskell, DataKinds, OverloadedStrings,
    DoAndIfThenElse#-}

module Quelea.Shim (
 runShimNode,
 runShimNodeWithOpts,
 mkDtLib
) where

import Quelea.Consts
import Quelea.Types
import Quelea.Consts
import Quelea.NameService.Types
import Quelea.Marshall
import Quelea.DBDriver
import Quelea.ShimLayer.Cache
import Quelea.ShimLayer.GC
import Quelea.Contract.Language

import Control.Concurrent (threadDelay)
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
import System.Posix.Process
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

debugPrint :: String -> IO ()
#ifdef DEBUG
debugPrint s = do
  tid <- myThreadId
  putStrLn $ "[" ++ (show tid) ++ "] " ++ s
#else
debugPrint _ = return ()
#endif

runShimNodeWithOpts :: OperationClass a
                    => GCSetting
                    -> Int -- fetch update interval
                    -> Double -- keep fraction
                    -> DatatypeLibrary a
                    -> [Server] -> Keyspace -- Cassandra connection info
                    -> NameService
                    -> IO ()
runShimNodeWithOpts gcSetting fetchUpdateInterval keepFraction dtLib serverList keyspace ns = do
  {- Connection to the Cassandra deployment -}
  pool <- newPool serverList keyspace Nothing
  {- Spawn cache manager -}
  cache <- initCacheManager pool fetchUpdateInterval keepFraction
  {- Spawn a pool of workers -}
  replicateM cNUM_WORKERS (forkIO $ worker dtLib pool cache gcSetting)
  case gcSetting of
    No_GC -> getServerJoin ns
    GC_Mem_Only -> getServerJoin ns
    otherwise -> do
      {- Join the broker to serve clients -}
      forkIO $ getServerJoin ns
      {- Start gcWorker -}
      gcWorker dtLib cache

runShimNode :: OperationClass a
            => DatatypeLibrary a
            -> [Server] -> Keyspace -- Cassandra connection info
            -> NameService
            -> IO ()
runShimNode = runShimNodeWithOpts GC_Full cCACHE_THREAD_DELAY cKEEP_FRACTION

worker :: OperationClass a => DatatypeLibrary a -> Pool -> CacheManager -> GCSetting -> IO ()
worker dtLib pool cache gcSetting = do
  ctxt <- ZMQ.context
  sock <- ZMQ.socket ctxt ZMQ.Rep
  pid <- getProcessID
  -- debugPrint "worker: connecting..."
  ZMQ.connect sock $ "ipc:///tmp/quelea/" ++ show pid
  -- debugPrint "worker: connected"
  {- loop forver servicing clients -}
  forever $ do
    binReq <- ZMQ.receive sock
    txns <- includedTxns cache
    case decodeOperationPayload binReq of
      ReqOper req -> do
        {- Fetch the operation from the datatype library using the object type and
        - operation name. -}
        let (op,av) =
              case dtLib ^. avMap ^.at (req^.objTypeReq, req^.opReq) of
                Nothing -> error $ "Not found in DatatypeLibrary:" ++ (show (req^.objTypeReq, req^.opReq))
                Just x -> x
        -- debugPrint $ "worker: before " ++ show (req^.objTypeReq, req^.opReq, av)
        (result, ctxtSize) <- case av of
          Eventual -> doOp op cache req ONE
          Causal -> processCausalOp req op cache
          Strong -> processStrongOp req op cache pool
        ZMQ.send sock [] $ encode result
        -- debugPrint $ "worker: after " ++ show (req^.objTypeReq, req^.opReq)
        -- Maybe perform summarization
        let gcFun =
              case dtLib ^. sumMap ^.at (req^.objTypeReq) of
                Nothing -> error "Worker(2)"
                Just x -> x
        case gcSetting of
          No_GC -> return ()
          otherwise -> maybeGCCache cache (req^.objTypeReq) (req^.keyReq) ctxtSize gcFun
        return ()
      ReqTxnCommit txid deps -> do
        -- debugPrint $ "Committing transaction " ++ show txid
        when (S.size deps > 0) $ runCas pool $ insertTxn txid deps
        ZMQ.send sock [] $ encode ResCommit
      ReqSnapshot objs -> do
        fetchUpdates cache ALL $ S.toList objs
        snapshot <- snapshotCache cache
        let filteredSnapshot = M.foldlWithKey (\m k v ->
              if S.member k objs then M.insert k v m else m) M.empty snapshot
        ZMQ.send sock [] $ encode $ ResSnapshot filteredSnapshot
  where
    processCausalOp req op cache =
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
            processCausalOp req op cache
    processStrongOp req op cache pool = do
      let (ot, k, sid) = (req^.objTypeReq, req^.keyReq, req^.sidReq)
      -- Get Lock
      getLock ot k sid pool
      -- debugPrint $ "processStrongOp: obtained lock"
      -- Read latest values at the key - under ALL
      fetchUpdates cache ALL [(ot,k)]
      -- Perform the op
      res <- doOp op cache req ALL
      -- Release Lock
      releaseLock ot k sid pool
      return res

doOp :: OperationClass a => GenOpFun -> CacheManager -> OperationPayload a -> Consistency -> IO (Response, Int)
doOp op cache request const = do
  let (OperationPayload objType key operName arg sessid seqno mbtxid getDeps) = request
  -- Build the context
  (ctxt, deps) <- buildContext objType key mbtxid
  -- Perform the operation on this context
  -- debugPrint $ "doOp: length of context = " ++ show (Prelude.length ctxt)
  let (res, effM) = op ctxt arg
  -- Add current location to the ones for which updates will be fetched
  addHotLocation cache objType key
  let resDeps = if getDeps then deps else S.empty
  result <- case effM of
    Nothing -> do
      return $ ResOper seqno res Nothing Nothing resDeps
    Just eff -> do
      -- Write effect writes to DB, and potentially to cache
      writeEffect cache objType key (Addr sessid (seqno+1)) eff deps const $ sel1 <$> mbtxid
      case mbtxid of
        Nothing -> return $ ResOper (seqno + 1) res Nothing Nothing resDeps
        Just (_,MAV_TxnPl _ _) -> do
          txns <- getInclTxnsAt cache objType key
          return $ ResOper (seqno + 1) res (Just eff) (Just txns) resDeps
        otherwise -> return $ ResOper (seqno + 1) res (Just eff) Nothing resDeps
  -- return response
  return (result, Prelude.length ctxt)
  where
    buildContext ot k Nothing = getContext cache ot k
    buildContext ot k (Just (_, RC_TxnPl l)) = do
      (ctxtVanilla, depsVanilla) <- buildContext ot k Nothing
      let (el, as) = S.foldl (\(el,as) (addr, eff) ->
                      (eff:el, S.insert addr as)) ([],S.empty) l
      return (el ++ ctxtVanilla, S.union as depsVanilla)
    buildContext ot k (Just (txid, MAV_TxnPl l txndeps)) = do
      res <- doesCacheIncludeTxns cache txndeps
      if res then buildContext ot k (Just (txid,RC_TxnPl l))
      else do
        fetchTxns cache txndeps
        buildContext ot k (Just (txid, MAV_TxnPl l txndeps))
    buildContext ot k (Just (_,RR_TxnPl effSet)) = return $
      S.foldl (\(el,as) (addr, eff) -> (eff:el, S.insert addr as))
              ([], S.empty) effSet


mkDtLib :: OperationClass a => [(a, (GenOpFun, GenSumFun), Availability)] -> DatatypeLibrary a
mkDtLib l =
  let (m1, m2) = Prelude.foldl core (M.empty, M.empty) l
  in DatatypeLibrary m1 m2
  where
    core (m1, m2) (op, (fun1, fun2), av) =
      (M.insert (getObjType op, op) (fun1, av) m1,
       M.insert (getObjType op) fun2 m2)


