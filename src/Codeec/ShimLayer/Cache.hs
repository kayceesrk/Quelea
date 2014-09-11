{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell, DataKinds, OverloadedStrings, DoAndIfThenElse  #-}

module Codeec.ShimLayer.Cache (
  CacheManager,

  initCacheManager,
  getContext,
  addEffectToCache,
  addHotLocation
) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString hiding (map, pack, putStrLn)
import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map.Lens
import Control.Monad (forever)
import Data.Maybe (fromJust)
import Control.Monad (replicateM,foldM)
import Database.Cassandra.CQL

import Codeec.Types
import Codeec.DBDriver

type Effect = ByteString

type CacheMap a = (M.Map (ObjType, Key) (S.Set (SessUUID, SeqNo, a, Effect)))
type Cache a    = MVar (CacheMap a)
type HotLocs    = MVar (S.Set (ObjType, Key))
type Semaphore  = MVar ()

data CacheManager a = CacheManager {
  _cacheMVar   :: Cache a,
  _hotLocsMVar :: HotLocs,
  _semMVar     :: Semaphore
}

makeLenses ''CacheManager

signalGenerator :: Semaphore -> IO ()
signalGenerator semMVar = forever $ do
  isEmpty <- isEmptyMVar semMVar
  if isEmpty
  then tryPutMVar semMVar ()
  else return True
  threadDelay 1000000 -- 1 second

addHotLocation :: OperationClass a => CacheManager a -> ObjType -> Key -> IO ()
addHotLocation cm ot k = do
  hotLocs <- takeMVar $ cm^.hotLocsMVar
  putMVar (cm^.hotLocsMVar) $ S.insert (ot,k) hotLocs

cacheMgrCore :: OperationClass a => CacheManager a -> Pool -> IO ()
cacheMgrCore (CacheManager cacheMVar hotLocsMVar semMVar) pool = forever $ do
  takeMVar semMVar
  -- Woken up. Read the current list of hot locations, and empty the MVar.
  locs <- takeMVar hotLocsMVar
  putMVar hotLocsMVar S.empty
  -- Read the database to fetch the all (including new remote) updates for the
  -- hot locations.
  newCache <- foldM (\cache (ot, k) -> readDB ot k >>= \v -> return $ M.insert (ot,k) v cache) M.empty (S.toList locs)
  -- Read the current cache
  curCache <- readMVar cacheMVar
  -- Get remote updates by mapping over newCache (smaller set of keys) and for
  -- each value, which is the set of effects, remove an effect from the set, it
  -- if the current cache contains it.
  let remoteUpdates = M.mapWithKey (\k v1 ->
        case M.lookup k curCache of
          Nothing -> v1
          Just v2 -> S.difference v1 v2) newCache
  -- Lock the current cache (takeMVar)
  curCache <- takeMVar cacheMVar
  let finalCache = M.unionWith S.union curCache remoteUpdates
  -- install the final cache
  putMVar cacheMVar finalCache
  where
    readDB ot k = runCas pool $ do
      rows <- cqlRead ot k
      -- TODO: Utilize dependencies
      let filteredRows = map (\(sid,sqn,_,op,v) -> (sid, sqn, toEnum op, v)) rows
      return $ S.fromList filteredRows

initCacheManager :: OperationClass a => Pool -> IO (CacheManager a)
initCacheManager pool = do
  cache <- newMVar M.empty
  hotLocs <- newMVar S.empty
  sem <- newEmptyMVar
  forkIO $ signalGenerator sem
  let cm = CacheManager cache hotLocs sem
  forkIO $ cacheMgrCore cm pool
  return $ cm

addEffectToCache :: OperationClass a => CacheManager a -> ObjType -> Key
                 -> SessUUID -> SeqNo -> a -> Effect -> IO ()
addEffectToCache cm ot k sid sqn op eff = do
  cache <- takeMVar $ cm^.cacheMVar
  putMVar (cm^.cacheMVar) $ M.insertWith (\a b -> S.union a b) (ot,k) (S.singleton (sid,sqn,op,eff)) cache

getContext :: OperationClass a => CacheManager a -> ObjType -> Key
           -> IO (S.Set (SessUUID, SeqNo, a, Effect))
getContext cm ot k = do
  cache <- readMVar $ cm^.cacheMVar
  case M.lookup (ot,k) cache of
    Nothing -> return S.empty
    Just s -> return s

-- TODO: Handle Sticky availability.
-- TODO: Unavailability.
