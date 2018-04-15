{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DoAndIfThenElse, BangPatterns  #-}

module Quelea.ShimLayer.GC (
  maybeGCCache,
  gcWorker
) where

import Quelea.Consts
import Quelea.Types
import Quelea.ShimLayer.Types
import Quelea.DBDriver
import Quelea.ShimLayer.UpdateFetcher

import Control.Monad.State
import Control.Lens
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Concurrent (threadDelay, myThreadId)
import Control.Concurrent.MVar
import System.Random (randomIO)
import Database.Cassandra.CQL
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Data.Time

makeLenses ''CacheManager
makeLenses ''DatatypeLibrary

-- #define DEBUG

debugPrint :: String -> IO ()
#ifdef DEBUG
debugPrint s = do
  tid <- myThreadId
  putStrLn $ "[" ++ (show tid) ++ "] " ++ s
#else
debugPrint _ = return ()
#endif


data VisitedState = Visited Bool  -- Boolean indicates whether the effect is resolved
                  | NotVisited (S.Set Addr)

data ResolutionState = ResolutionState {
  _keyCursor    :: M.Map SessID SeqNo,
  _visitedState :: M.Map Addr VisitedState
}

makeLenses ''ResolutionState

{- How to GC a transactional effect?
 -----------------------------------------
 - When GCing a set of effects {a@t1,b@t2} belonging to transactions t1 and t2,
 - one needs to ensure that the transaction markers are preserved after the GC.
 - Assume c is the new effect that summarizes a and b, and GCM is the gc
 - marker. Now the final state after GC will be:

    {c} --introduced by--> {GCM@{t1,t2}} --GCs--> {a,b}

 - where the set of transactions of the effects that were GCed are included in
 - the GC marker. Now, if c is included in any shim layer node, the
 - transactions t1 and t2 also need to be processed.
 -}


gcDB :: CacheManager -> ObjType -> Key -> GenSumFun -> IO ()
gcDB cm ot k gc = gcDBCore cm ot k gc 1

gcDBCore :: CacheManager -> ObjType -> Key -> GenSumFun -> Int -> IO ()
gcDBCore cm ot k gc 0 = return ()
gcDBCore cm ot k gc repeat = do
  debugPrint $ "gcDB: start"
  -- Allocate new session id
  gcSid <- SessID <$> randomIO
  -- Get GC lock
  getGCLock ot k gcSid $ cm^.pool
  -- Get time at the start of GC
  currentTime <- getCurrentTime
  let gcTime = currentTime
  lgctMap <- readMVar $ cm^.lastGCTimeMVar
  rows <- case M.lookup (ot,k) lgctMap of
            Nothing -> runCas (cm^.pool) $ cqlReadWithTime ot ALL k (cm^.keepFraction)
            Just lastGCTime -> runCas (cm^.pool) $ cqlReadAfterTimeWithTime ot ALL k lastGCTime (cm^.keepFraction)
  -- Split the rows into effects and gc markers
  let (effRows, gcMarker) = foldl (\(effAcc,gcAcc) (sid,sqn,time,deps,val,txnid) ->
        case txnid of
          Just _ -> (effAcc, gcAcc) -- XXX KC; Handle transactions
          Nothing ->
            case val of
              EffectVal bs -> ((sid,sqn,time,deps,bs):effAcc, gcAcc)
              GCMarker _ -> case gcAcc of
                            Nothing -> (effAcc, Just (sid,sqn,time,deps))
                            Just _ -> error "Multiple GC Markers") ([], Nothing) rows
  -- Build the GC cursor
  let gcCursor = case gcMarker of
                   Nothing -> M.empty
                   Just (sid, sqn, time, deps) ->
                     S.foldl (\m (Addr sid sqn) -> M.insert sid sqn m)
                             (M.singleton sid sqn) deps
  -- Build datastructure for filtering out unresolved effects
  let (effSet, depsMap) =
        foldl (\(setAcc, mapAcc) (sid, sqn, time, deps, eff) ->
                  (S.insert (Addr sid sqn, eff, time) setAcc,
                   M.insert (Addr sid sqn) (NotVisited deps) mapAcc))
          (S.empty, M.empty) effRows
  -- filter unresolved effects i,e) effects whose causal cut is also not visible.
  let filteredSet = filterUnresolved gcCursor depsMap effSet
  let (addrList, effList, timeList) = unzip3 (S.toList filteredSet)
  let gcedEffList = gc effList
  -- Allocate new gc address
  let gcAddr = Addr gcSid 1
  let (outRows, count) =
        foldl (\(acc, idx) eff ->
                  ((gcSid, idx, S.singleton gcAddr, EffectVal eff, Nothing):acc, idx+1))
              ([],2) gcedEffList
  runCas (cm^.pool) $ do
    -- Insert new effects
    mapM_ (\r -> cqlInsert ot ALL k r) outRows
    -- Delete previous marker if it exists
    case gcMarker of
      Nothing -> return ()
      Just (sid, sqn, time, _) -> cqlDelete ot k time sid sqn
    -- Insert marker
    let am = foldl (\m (Addr sid sqn) ->
               case M.lookup sid m of
                 Nothing -> M.insert sid sqn m
                 Just oldSqn -> M.insert sid (max oldSqn sqn) m) M.empty addrList
    let newCursor = M.unionWith max am gcCursor
    let newDeps = S.fromList $ map (\(sid,sqn) -> Addr sid sqn) $ M.toList newCursor
    cqlInsert ot ALL k (gcSid, 1, newDeps, GCMarker gcTime, Nothing)
    -- Update lastGCTime before deleting
    liftIO $ do
      lgctMap <- takeMVar $ cm^.lastGCTimeMVar
      putMVar (cm^.lastGCTimeMVar) $ M.insert (ot,k) gcTime lgctMap
    -- Delete old rows
    mapM_ (\(Addr sid sqn, time) -> cqlDelete ot k time sid sqn) $ zip addrList timeList
  {- info -}
  debugPrint $ "gcDB: inserted Rows=" ++ show (length outRows)
  debugPrint $ "gcDB: deleted Rows=" ++ show (length addrList)
  releaseGCLock ot k gcSid $ cm^.pool
  {- Remove the current object from diskRowCount map -}
  drc <- takeMVar $ cm^.diskRowCntMVar
  putMVar (cm^.diskRowCntMVar) $ M.delete (ot,k) drc
  debugPrint $ "gcDB: end"
  gcDBCore cm ot k gc $ repeat - 1

maybeGCCache :: CacheManager -> ObjType -> Key -> Int -> GenSumFun -> IO ()
maybeGCCache cm ot k curSize gc | curSize < cCACHE_LWM = return ()
                                | otherwise = do
  hwmMap <- readMVar $ cm^.hwmMVar
  let hwm = case M.lookup (ot,k) hwmMap of
              Nothing -> cCACHE_LWM
              Just x -> x
  when (curSize > hwm) $ do
    cache <- takeMVar $ cm^.cacheMVar
    let ctxt = case M.lookup (ot, k) cache of
                Nothing -> []
                Just s -> map (\(a,e) -> e) $ S.toList s
    let !newCtxt = gc ctxt
    newUUID <- SessID <$> randomIO
    let (newCache,_) = foldl (\(s,i) e -> (S.insert (Addr newUUID i, e) s, i+1)) (S.empty, 1) newCtxt
    hwm <- takeMVar $ cm^.hwmMVar
    putMVar (cm^.hwmMVar) $ M.insert (ot, k) (length newCtxt * 2) hwm
    putMVar (cm^.cacheMVar) $ M.insert (ot, k) newCache cache

filterUnresolved :: CursorAtKey                   -- Key Cursor
                 -> M.Map Addr VisitedState       -- Input visited state
                 -> S.Set (Addr, Effect, UTCTime) -- Unfiltered input
                 -> S.Set (Addr, Effect, UTCTime) -- Filtered result
filterUnresolved kc m1 s2 =
  let addrList = M.keys m1
      ResolutionState _ vs =
        foldl (\vs addr -> execState (isResolved addr) vs) (ResolutionState kc m1) addrList
  in S.filter (\(addr,_,_) ->
        case M.lookup addr vs of
          Nothing -> error "filterUnresolved: unexpected state(1)"
          Just (Visited True) -> True
          Just (Visited False) -> False
          Just (NotVisited _) -> error "filterUnresolved: unexpected state(2)") s2

isResolved :: Addr -> State ResolutionState Bool
isResolved addr = do
  vs <- use visitedState
  case M.lookup addr vs of
    Nothing -> do -- Might be an effect already in the cache
      let Addr sid sqn = addr
      if sqn == 0 -- Special case to please Cassandra
      then do
        visitedState .= M.insert addr (Visited True) vs
        return True
      else do
        kc <- use keyCursor
        case M.lookup sid kc of
          -- Session of effect not found in cache => effect unseen
          Nothing -> do
            visitedState .= M.insert addr (Visited False) vs
            return False
          -- Session found in cache, cache is sufficiently recent?
          Just maxSqn -> do
            let res = sqn <= maxSqn
            visitedState .= M.insert addr (Visited res) vs
            return res
    Just (Visited res) -> return res
    Just (NotVisited deps) -> do
      res <- foldM (\acc addr -> isResolved addr >>= \r -> return (r && acc)) True $ S.toList deps
      newVs <- use visitedState
      visitedState .= M.insert addr (Visited res) newVs
      return res

gcWorker :: OperationClass a => DatatypeLibrary a -> CacheManager -> IO ()
gcWorker dtLib cm = forever $ do
  threadDelay cGC_WORKER_THREAD_DELAY
  drc <- readMVar $ cm^.diskRowCntMVar
  let todoObjs = M.foldlWithKey (\todoObjs (ot,k) rowCount ->
                                    if rowCount > cDISK_LWM
                                    then (ot,k):todoObjs
                                    else todoObjs) [] drc
  mapM_ (\(ot,k) ->
    let gcFun = case dtLib ^. sumMap ^.at ot of {Nothing -> error "gcWorker"; Just x -> x}
    in gcDB cm ot k gcFun) todoObjs
