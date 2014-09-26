{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell, DataKinds, OverloadedStrings, DoAndIfThenElse  #-}

module Codeec.ShimLayer.Cache (
  CacheManager,

  initCacheManager,
  getContext,
  addHotLocation,
  writeEffect,
  doesCacheInclude,
  waitForCacheRefresh,
  fetchUpdates,
  maybeGCCache
) where

-- Minimum high water mark size for GC
#define LWM 64

import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString hiding (map, pack, putStrLn, foldl, length, filter)
import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map.Lens
import Control.Monad (forever, when, replicateM, foldM)
import Data.Maybe (fromJust)
import Database.Cassandra.CQL
import Control.Monad.State
import System.IO
import System.Random (randomIO)
import Control.Applicative ((<$>))

import Codeec.Types
import Codeec.ShimLayer.Types
import Codeec.DBDriver
import Codeec.ShimLayer.UpdateFetcher

makeLenses ''CacheManager

maybeGCCache :: CacheManager -> ObjType -> Key -> Int -> GenSumFun -> IO ()
maybeGCCache cm ot k curSize gc | curSize < LWM = return ()
                           | otherwise = do
  cache <- takeMVar $ cm^.cacheMVar
  let ctxt = case M.lookup (ot, k) cache of
               Nothing -> []
               Just s -> map (\(a,e) -> e) $ S.toList s
  let newCtxt = gc ctxt
  newUUID <- randomIO
  let (newCache,_) = foldl (\(s,i) e -> (S.insert (Addr newUUID i, e) s, i+1)) (S.empty, 1) newCtxt
  hwm <- takeMVar $ cm^.hwmMVar
  putMVar (cm^.hwmMVar) $ M.insert (ot, k) (length newCtxt * 2) hwm
  putMVar (cm^.cacheMVar) $ M.insert (ot, k) newCache cache
  putStrLn $ "maybeGCCache : finalSize=" ++ (show $ length newCtxt)

gcDB :: CacheManager -> ObjType -> Key -> GenSumFun -> IO ()
gcDB cm ot k gc = do
  sid <- randomIO
  rows <- runCas (cm^.pool) $ cqlRead ot ALL k
  -- get causal cut (rows)
  -- remove GC markers (rows)
  -- filter effects from unfinished sessions (rows)
  releaseLock ot k sid $ cm^.pool

initCacheManager :: Pool -> IO CacheManager
initCacheManager pool = do
  cache <- newMVar M.empty
  hwm <- newMVar M.empty
  cursor <- newMVar M.empty
  nearestDeps <- newMVar M.empty
  hotLocs <- newMVar S.empty
  sem <- newEmptyMVar
  blockedList <- newMVar []
  forkIO $ signalGenerator sem
  let cm = CacheManager cache hwm cursor nearestDeps hotLocs sem blockedList pool
  forkIO $ cacheMgrCore cm
  return $ cm
  where
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

writeEffect :: CacheManager -> ObjType -> Key -> Addr -> Effect -> S.Set Addr -> Consistency -> IO ()
writeEffect cm ot k addr eff origDeps const = do
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
  runCas (cm^.pool) $ cqlWrite ot const k (sid, sqn, deps, EffectVal eff)

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
