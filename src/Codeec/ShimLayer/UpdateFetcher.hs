{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DoAndIfThenElse  #-}

module Codeec.ShimLayer.UpdateFetcher (
  fetchUpdates
) where

import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Control.Monad.State
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

getEffectsCore :: Consistency -> CursorMap -> Pool -> (CacheMap, NearestDepsMap)
               -> (ObjType,Key) -> IO (CacheMap, NearestDepsMap)
getEffectsCore const cursor pool acc (ot,k) = do
    -- Read the database
    rows <- runCas pool $ cqlRead ot const k
    -- Filter effects that were already seen
    let cursorAtKey = case M.lookup (ot,k) cursor of {Nothing -> M.empty; Just m -> m}
    let unseenRows = filter (\(sid,sqn,_,_) -> isUnseenEffect cursorAtKey sid sqn) rows
    if unseenRows == []
    then return acc
    else do
      let effectRows = foldl (\acc v@(a,b,c,d) ->
            case d of {EffectVal bs -> (a,b,c,bs):acc; otherwise -> acc}) [] unseenRows
      -- Build datastructures
      let (effSet, depsMap) = foldl (\(s1, m2) (sid,sqn,deps,eff) ->
            (S.insert (Addr sid sqn, eff) s1,
              M.insert (Addr sid sqn) (NotVisited deps) m2)) (S.empty, M.empty) effectRows
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
