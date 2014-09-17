{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell, DataKinds, OverloadedStrings, DoAndIfThenElse  #-}

module Codeec.ShimLayer.Cache (
  CacheManager,

  initCacheManager,
  getContext,
  addHotLocation,
  writeEffect,
  doesCacheInclude,
  waitForCacheRefresh,
  fetchUpdates
) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString hiding (map, pack, putStrLn)
import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map.Lens
import Control.Monad (forever, when, replicateM, foldM)
import Data.Maybe (fromJust)
import Database.Cassandra.CQL
import Control.Monad.State
import System.IO

import Codeec.Types
import Codeec.DBDriver

type Effect = ByteString

type CacheMap    = (M.Map (ObjType, Key) (S.Set (Addr, Effect)))
type Cache       = MVar CacheMap
type CursorMap   = (M.Map (ObjType, Key) (M.Map SessUUID SeqNo))
type Cursor      = MVar CursorMap
type NearestDepsMap = (M.Map (ObjType, Key) (S.Set Addr))
type NearestDeps = MVar NearestDepsMap
type HotLocs     = MVar (S.Set (ObjType, Key))
type Semaphore   = MVar ()
type ThreadQueue = MVar ([MVar ()])

data CacheManager = CacheManager {
  _cacheMVar   :: Cache,
  _cursorMVar  :: Cursor,
  _depsMVar    :: NearestDeps,
  _hotLocsMVar :: HotLocs,
  _semMVar     :: Semaphore,
  _blockedMVar :: ThreadQueue,
  _pool        :: Pool
}

makeLenses ''CacheManager
makeLenses ''Addr

data VisitedState = Visited Bool  -- Boolean indicates whether the effect is resolved
                  | NotVisited (S.Set Addr)

data ResolutionState = ResolutionState {
  _keyCursor    :: M.Map SessUUID SeqNo,
  _visitedState :: M.Map Addr VisitedState
}

makeLenses ''ResolutionState

signalGenerator :: Semaphore -> IO ()
signalGenerator semMVar = forever $ do
  isEmpty <- isEmptyMVar semMVar
  if isEmpty
  then tryPutMVar semMVar ()
  else return True
  threadDelay 1000000 -- 1 second

addHotLocation :: CacheManager -> ObjType -> Key -> IO ()
addHotLocation cm ot k = do
  hotLocs <- takeMVar $ cm^.hotLocsMVar
  putMVar (cm^.hotLocsMVar) $ S.insert (ot,k) hotLocs

cacheMgrCore :: CacheManager -> IO ()
cacheMgrCore cm = forever $ do
  takeMVar $ cm^.semMVar
  -- Woken up. Read the current list of hot locations, and empty the MVar.
  locs <- takeMVar $ cm^.hotLocsMVar
  putMVar (cm^.hotLocsMVar) S.empty
  -- Fetch updates
  fetchUpdates ONE cm $ S.toList locs
  -- Wakeup threads that are waiting for the cache to be refreshed
  blockedList <- takeMVar $ cm^.blockedMVar
  putMVar (cm^.blockedMVar) []
  mapM_ (\mv -> putMVar mv ()) blockedList

fetchUpdates :: Consistency -> CacheManager -> [(ObjType, Key)] -> IO ()
fetchUpdates const cm locs = do
  cursor <- readMVar $ cm^.cursorMVar
  (newEffMap, newAddrMap) <- foldM (getEffectsCore const cursor (cm^.pool)) (M.empty, M.empty) locs
  -- Update cache state
  cache <- takeMVar $ cm^.cacheMVar
  cursor <- takeMVar $ cm^.cursorMVar
  deps <- takeMVar $ cm^.depsMVar
  let newCache = M.unionWith S.union newEffMap cache
  putMVar (cm^.cacheMVar) newCache
  let newDeps = M.unionWith S.union newAddrMap deps
  -- Create a new cursor map
  let newCursorMap = M.map (S.foldl (\m (Addr sid sqn) ->
          case M.lookup sid m of
            Nothing -> M.insert sid sqn m
            Just oldSqn -> if oldSqn < sqn then M.insert sid sqn m else m) M.empty) deps
  -- merge cursors
  let newCursor = M.unionWith (M.unionWith max) newCursorMap cursor
  putMVar (cm^.cursorMVar) newCursor
  putMVar (cm^.depsMVar) newDeps

getEffectsCore :: Consistency -> CursorMap -> Pool
               -> (CacheMap, NearestDepsMap) -> (ObjType,Key) -> IO (CacheMap, NearestDepsMap)
getEffectsCore const cursor pool acc (ot,k) = do
    -- Read the database
    rows <- runCas pool $ cqlRead ot const k
    -- Filter effects that were already seen
    let cursorAtKey = case M.lookup (ot,k) cursor of {Nothing -> M.empty; Just m -> m}
    let unseenRows = Prelude.filter (\(sid,sqn,_,_) -> isUnseenEffect cursorAtKey sid sqn) rows
    if unseenRows == []
    then return acc
    else do
      -- Build datastructures
      let (effSet, depsMap) = Prelude.foldl (\(s1, m2) (sid,sqn,deps,eff) ->
            (S.insert (Addr sid sqn, eff) s1,
              M.insert (Addr sid sqn) (NotVisited deps) m2)) (S.empty, M.empty) unseenRows
      let filteredSet = filterUnresolved cursorAtKey depsMap effSet
      let (newEffMap, newAddrMap) = acc
      let newEffSet = filteredSet
      let newAddrSet = S.map (\(addr, _) -> addr) filteredSet
      return $ (M.insert (ot,k) newEffSet newEffMap, M.insert (ot,k) newAddrSet newAddrMap)
  where
    isUnseenEffect cursorAtKey sid sqn =
      case M.lookup sid cursorAtKey of
        Nothing -> True
        Just cursorSqn -> sqn > cursorSqn

filterUnresolved :: M.Map SessUUID SeqNo    -- Key Cursor
             -> M.Map Addr VisitedState -- Input visited state
             -> S.Set (Addr, Effect)    -- Unfiltered input
             -> S.Set (Addr, Effect)    -- Filtered result
filterUnresolved kc m1 s2 =
  let addrList = M.keys m1
      ResolutionState _ vs = Prelude.foldl (\vs addr -> execState (isResolved addr) vs) (ResolutionState kc m1) addrList
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

initCacheManager :: Pool -> IO CacheManager
initCacheManager pool = do
  cache <- newMVar M.empty
  cursor <- newMVar M.empty
  nearestDeps <- newMVar M.empty
  hotLocs <- newMVar S.empty
  sem <- newEmptyMVar
  blockedList <- newMVar []
  forkIO $ signalGenerator sem
  let cm = CacheManager cache cursor nearestDeps hotLocs sem blockedList pool
  forkIO $ cacheMgrCore cm
  return $ cm

-- Returns the set of effects at the location and a set of nearest dependencies
-- for this location.
getContext :: CacheManager -> ObjType -> Key -> IO ([Effect], S.Set Addr)
getContext cm ot k = do
  cache <- takeMVar $ cm^.cacheMVar
  deps <- takeMVar $ cm^.depsMVar
  putMVar (cm^.cacheMVar) cache
  putMVar (cm^.depsMVar) deps
  let v1 = case M.lookup (ot,k) cache of
             Nothing -> []
             Just s -> Prelude.map (\(a,e) -> e) (S.toList s)
  let v2 = case M.lookup (ot,k) deps of {Nothing -> S.empty; Just s -> s}
  return (v1, v2)

writeEffect :: CacheManager -> ObjType -> Key -> Addr -> Effect -> S.Set Addr -> IO ()
writeEffect cm ot k addr eff origDeps = do
  let Addr sid sqn = addr
  -- Empty dependence set causes error with Cassandra serialization. Following circumvents it.
  let deps = if S.size origDeps == 0 then (S.fromList [Addr sid 0]) else origDeps
  -- Does cache include the previous effect?
  isPrevEffectAvailable <- doesCacheInclude cm ot k sid (sqn - 1)
  -- Only write to cache if the previous effect is available in the cache. This
  -- maintains the cache to be a causally consistent cut of the updates.
  when (sqn == 1 || isPrevEffectAvailable) $ do
    cache <- takeMVar $ cm^.cacheMVar
    cursor <- takeMVar $ cm^.cursorMVar
    -- curDeps may be different from the deps seen before the operation was performed.
    curDeps <- takeMVar $ cm^.depsMVar
    -- Update cache
    putMVar (cm^.cacheMVar) $ M.insertWith S.union (ot,k) (S.singleton (addr, eff)) cache
    -- Update cursor
    let cursorAtKey = case M.lookup (ot,k) cursor of {Nothing -> M.empty; Just m -> m}
    let newCursorAtKey = M.insert sid sqn cursorAtKey
    putMVar (cm^.cursorMVar) $ M.insert (ot,k) newCursorAtKey cursor
    -- Update dependence
    putMVar (cm^.depsMVar) $ M.insertWith S.union (ot,k) (S.singleton addr) curDeps
  -- Write to database
  runCas (cm^.pool) $ cqlWrite ot ONE k (sid, sqn, deps, eff)

doesCacheInclude :: CacheManager -> ObjType -> Key -> SessUUID -> SeqNo -> IO Bool
doesCacheInclude cm ot k sid sqn = do
  cursor <- readMVar $ cm^.cursorMVar
  case M.lookup (ot,k) cursor of
    Nothing -> return False
    Just cursorAtKey ->
      case M.lookup sid cursorAtKey of
        Nothing -> return False
        Just curSqn -> return $ (==) sqn curSqn

waitForCacheRefresh :: CacheManager -> ObjType -> Key -> IO ()
waitForCacheRefresh cm ot k = do
  hotLocs <- takeMVar $ cm^.hotLocsMVar
  blockedList <- takeMVar $ cm^.blockedMVar
  mv <- newEmptyMVar
  putMVar (cm^.hotLocsMVar) $ S.insert (ot,k) hotLocs
  putMVar (cm^.blockedMVar) $ mv:blockedList
  takeMVar mv
