{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DoAndIfThenElse  #-}

module Codeec.ShimLayer.GC (
  maybeGCCache
) where

import Codeec.Types
import Codeec.ShimLayer.Types
import Codeec.DBDriver
import Codeec.ShimLayer.UpdateFetcher

import Control.Lens
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Concurrent.MVar
import System.Random (randomIO)
import Database.Cassandra.CQL

makeLenses ''CacheManager

-- Minimum high water mark size for GC
#define LWM 64

gcDB :: CacheManager -> ObjType -> Key -> GenSumFun -> IO ()
gcDB cm ot k gc = do
  -- Allocate new session id
  gcSid <- randomIO
  getLock ot k gcSid $ cm^.pool
  rows <- runCas (cm^.pool) $ cqlRead ot ALL k
  -- Split the rows into effects and gc markers
  let (effRows, gcMarker) = foldl (\(effAcc,gcAcc) (sid,sqn,deps,val) ->
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
                  ((gcSid, idx, S.singleton gcAddr, EffectVal eff):acc, idx+1))
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
    cqlInsert ot ALL k (gcSid, 1, newDeps, GCMarker)
    -- Delete old rows
    mapM_ (\(Addr sid sqn) -> cqlDelete ot k sid sqn) addrList
  releaseLock ot k gcSid $ cm^.pool

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
