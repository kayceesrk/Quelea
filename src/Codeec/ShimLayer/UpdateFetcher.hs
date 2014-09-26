{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DoAndIfThenElse  #-}

module Codeec.ShimLayer.UpdateFetcher (
  fetchUpdates
) where

import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Control.Monad.State
import Control.Monad (when)
import Database.Cassandra.CQL
import Control.Concurrent

import Codeec.Types
import Codeec.ShimLayer.Types
import Codeec.DBDriver

data VisitedState = Visited Bool  -- Boolean indicates whether the effect is resolved
                  | NotVisited (S.Set Addr)

data ResolutionState = ResolutionState {
  _keyCursor    :: M.Map SessUUID SeqNo,
  _visitedState :: M.Map Addr VisitedState
}

makeLenses ''CacheManager
makeLenses ''Addr
makeLenses ''ResolutionState

fetchUpdates :: CacheManager -> Consistency -> [(ObjType, Key)] -> IO ()
fetchUpdates cm const = mapM_ $ fetchUpdate cm const

fetchUpdate :: CacheManager -> Consistency -> (ObjType, Key) -> IO ()
fetchUpdate cm const (ot, k) = do
  -- Read the database
  rows <- runCas (cm^.pool) $ cqlRead ot const k
  -- Split the rows into effects and gc markers
  let (effRows, gcMarkerRows) = foldl (\(effAcc,gcAcc) (sid,sqn,deps,val) ->
        case val of
          EffectVal bs -> ((sid,sqn,deps,bs):effAcc, gcAcc)
          GCMarker -> (effAcc, (sid,sqn,deps):gcAcc)) ([], []) rows
  -- GC markers?
  when (length gcMarkerRows /= 0) (error "GC Markers: unexpected!")
  cursor <- readMVar $ cm^.cursorMVar
  let cursorAtKey = case M.lookup (ot, k) cursor of
                      Nothing -> M.empty
                      Just m -> m
  -- Filter effects that were already seen.
  let unseenRows = filter (\(sid, sqn, _, _) -> isUnseen cursorAtKey (Addr sid sqn)) effRows
  -- Build datastructure for filtering out unresolved effects
  let (effSet, depsMap) =
        foldl (\(setAcc, mapAcc) (sid, sqn, deps, eff) ->
                  (S.insert (Addr sid sqn, eff) setAcc,
                   M.insert (Addr sid sqn) (NotVisited deps) mapAcc))
          (S.empty, M.empty) unseenRows
  -- filter unresolved effects i,e) effects whose causal cut is also not visible.
  let filteredSet = filterUnresolved cursorAtKey depsMap effSet
  -- Update cache state
  cache <- takeMVar $ cm^.cacheMVar
  cursor <- takeMVar $ cm^.cursorMVar
  deps <- takeMVar $ cm^.depsMVar
  -- Update cache
  let newCache = M.unionWith S.union cache $ M.singleton (ot,k) filteredSet
  putMVar (cm^.cacheMVar) newCache
  -- Update cursor
  let cursorAtKey = case M.lookup (ot, k) cursor of {Nothing -> M.empty; Just m -> m}
  let newCursorAtKey = S.foldl (\m (Addr sid sqn, _) ->
                          case M.lookup sid m of
                            Nothing -> M.insert sid sqn m
                            Just oldSqn -> if oldSqn < sqn
                                           then M.insert sid sqn m
                                           else m) cursorAtKey filteredSet
  let newCursor = M.insert (ot,k) newCursorAtKey cursor
  putMVar (cm^.cursorMVar) newCursor
  -- Update dependence
  let newDeps = M.unionWith S.union deps $ M.singleton (ot,k) (S.map (\(a,_) -> a) filteredSet)
  putMVar (cm^.depsMVar) newDeps

-- Returns true if the given effect is not encapsulated by the cursor
isUnseen :: CursorAtKey -> Addr -> Bool
isUnseen cak (Addr sid sqn) =
  case M.lookup sid cak of
    Nothing -> True
    Just cursorSqn -> sqn > cursorSqn

-- Combines curors at a particular key. Since a cursor at a given (key, sessid)
-- records the largest sequence number seen so far, given two cursors at some
-- key k, the merge operation picks the larger of the sequence numbers for each
-- sessid.
mergeCursorsAtKey :: CursorAtKey -> CursorAtKey -> CursorAtKey
mergeCursorsAtKey = M.unionWith max

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
