{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DoAndIfThenElse, BangPatterns  #-}

module Codeec.ShimLayer.GC (
  maybeGCCache,
  gcWorker
) where

import Codeec.Types
import Codeec.ShimLayer.Types
import Codeec.DBDriver
import Codeec.ShimLayer.UpdateFetcher

import Control.Monad.State
import Control.Lens
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import System.Random (randomIO)
import Database.Cassandra.CQL
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)

makeLenses ''CacheManager
makeLenses ''DatatypeLibrary

-- Minimum high water mark size for GC
#define CACHE_LWM 128
#define DISK_LWM 256

data VisitedState = Visited Bool  -- Boolean indicates whether the effect is resolved
                  | NotVisited (S.Set Addr)

data ResolutionState = ResolutionState {
  _keyCursor    :: M.Map SessID SeqNo,
  _visitedState :: M.Map Addr VisitedState
}

makeLenses ''ResolutionState

{- TODO: How to GC a transactional effect?
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
gcDB cm ot k gc = do
  -- Allocate new session id
  gcSid <- SessID <$> randomIO
  getLock ot k gcSid $ cm^.pool
  rows <- runCas (cm^.pool) $ cqlRead ot ALL k
  -- Split the rows into effects and gc markers
  let (effRows, gcMarker) = foldl (\(effAcc,gcAcc) (sid,sqn,deps,val,txnid) ->
        case txnid of
          Just _ -> error "gcDB: Cannot handle transactions yet! TODO."
          Nothing ->
            case val of
              EffectVal bs -> ((sid,sqn,deps,bs):effAcc, gcAcc)
              GCMarker -> case gcAcc of
                            Nothing -> (effAcc, Just (sid,sqn,deps))
                            Just _ -> error "Multiple GC Markers") ([], Nothing) rows
  -- Build the GC cursor
  let gcCursor = case gcMarker of
                   Nothing -> M.empty
                   Just (sid, sqn, deps) ->
                     S.foldl (\m (Addr sid sqn) -> M.insert sid sqn m)
                             (M.singleton sid sqn) deps
  -- Build datastructure for filtering out unresolved effects
  let (effSet, depsMap) =
        foldl (\(setAcc, mapAcc) (sid, sqn, deps, eff) ->
                  (S.insert (Addr sid sqn, eff) setAcc,
                   M.insert (Addr sid sqn) (NotVisited deps) mapAcc))
          (S.empty, M.empty) effRows
  -- filter unresolved effects i,e) effects whose causal cut is also not visible.
  let filteredSet = filterUnresolved gcCursor depsMap effSet
  let (addrList, effList) = unzip (S.toList filteredSet)
  let gcedEffList = gc effList
  -- Allocate new gc address
  let gcAddr = Addr gcSid 1
  let (outRows, count) =
        foldl (\(acc, idx) eff ->
                  ((gcSid, idx, S.singleton gcAddr, EffectVal eff, Nothing):acc, idx+1))
              ([],2) gcedEffList
  putStrLn $ "gcDB: count=" ++ show (count-2)
  runCas (cm^.pool) $ do
    -- Insert new effects
    mapM_ (\r -> cqlInsert ot ALL k r) outRows
    -- Delete previous marker if it exists
    case gcMarker of
      Nothing -> return ()
      Just (sid, sqn, _) -> cqlDelete ot k sid sqn
    -- Insert marker
    let am = foldl (\m (Addr sid sqn) ->
               case M.lookup sid m of
                 Nothing -> M.insert sid sqn m
                 Just oldSqn -> M.insert sid (max oldSqn sqn) m) M.empty addrList
    let newCursor = M.unionWith max am gcCursor
    let newDeps = S.fromList $ map (\(sid,sqn) -> Addr sid sqn) $ M.toList newCursor
    cqlInsert ot ALL k (gcSid, 1, newDeps, GCMarker, Nothing)
    -- Delete old rows
    mapM_ (\(Addr sid sqn) -> cqlDelete ot k sid sqn) addrList
  releaseLock ot k gcSid $ cm^.pool
  {- Remove the current object from diskRowCount map -}
  drc <- takeMVar $ cm^.diskRowCntMVar
  putMVar (cm^.diskRowCntMVar) $ M.delete (ot,k) drc

maybeGCCache :: CacheManager -> ObjType -> Key -> Int -> GenSumFun -> IO ()
maybeGCCache cm ot k curSize gc | curSize < CACHE_LWM = return ()
                                | otherwise = do
  hwmMap <- readMVar $ cm^.hwmMVar
  let hwm = case M.lookup (ot,k) hwmMap of
              Nothing -> CACHE_LWM
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

filterUnresolved :: CursorAtKey             -- Key Cursor
                 -> M.Map Addr VisitedState -- Input visited state
                 -> S.Set (Addr, Effect)    -- Unfiltered input
                 -> S.Set (Addr, Effect)    -- Filtered result
filterUnresolved kc m1 s2 =
  let addrList = M.keys m1
      ResolutionState _ vs =
        foldl (\vs addr -> execState (isResolved addr) vs) (ResolutionState kc m1) addrList
  in S.filter (\(addr,eff) ->
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
  threadDelay 1000000
  drc <- readMVar $ cm^.diskRowCntMVar
  let todoObjs = M.foldlWithKey (\todoObjs (ot,k) rowCount ->
                                    if rowCount > DISK_LWM
                                    then (ot,k):todoObjs
                                    else todoObjs) [] drc
  print todoObjs
  mapM_ (\(ot,k) ->
    let gcFun = fromJust $ dtLib ^. sumMap ^.at ot
    in gcDB cm ot k gcFun) todoObjs


