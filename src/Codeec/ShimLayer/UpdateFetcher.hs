{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DoAndIfThenElse  #-}

module Codeec.ShimLayer.UpdateFetcher (
  fetchUpdates
) where

import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Database.Cassandra.CQL
import Control.Monad.Trans.State
import Control.Monad.Trans (liftIO)
import Control.Monad (mapM_, when, foldM)
import Data.ByteString (empty)
import Data.Maybe (fromJust)

import Codeec.Types
import Codeec.ShimLayer.Types
import Codeec.DBDriver

makeLenses ''CacheManager
makeLenses ''Addr

data CollectRowsState = CollectRowsState {
  _cursorCRS       :: CursorMap,
  _includedTxnsCRS :: S.Set TxnID,
  _todoObjs        :: S.Set (ObjType, Key),
  _rowsMap         :: M.Map (ObjType, Key) [ReadRow],
  _processedTxns   :: M.Map TxnID (S.Set TxnDep)
}

makeLenses ''CollectRowsState
makeLenses ''TxnDep

data VisitedState = Visited (Maybe Effect)
                  | NotVisited Effect (S.Set Addr) (S.Set TxnDep)
                  | Visiting

data ResolutionState = ResolutionState {
  _cursor       :: CursorMap,
  _visitedState :: M.Map (ObjType, Key) (M.Map Addr VisitedState)
}

makeLenses ''ResolutionState

fetchUpdates :: CacheManager -> Consistency -> [(ObjType, Key)] -> IO ()
fetchUpdates cm const todoList = do
  -- Recursively read the DB and collect all the rows
  cursor <- readMVar $ cm^.cursorMVar
  inclTxns <- readMVar $ cm^.includedTxnsMVar
  let todoObjs = S.fromList todoList
  let crs = CollectRowsState cursor inclTxns todoObjs M.empty M.empty
  CollectRowsState _ _ _ rowsMap procTransMap <- execStateT (collectTransitiveRows (cm^.pool) const) crs
  -- Construct uncovered effects and cursor (constructed from cursor in cache
  -- as well as any gc cursor, if present)
  let (effRowsMap, cursorMap, gcMarkerMap) = M.foldlWithKey (\(erm, curm, gcmm) (ot,k) rowList ->
         let (er,cur,gcm) = foldl (\(er, cur, gcm) (sid,sqn,deps,val,mbTxid) ->
                case val of
                  EffectVal bs ->
                    if isCovered cursor ot k sid sqn
                    then (er, cur, gcm)
                    else
                      let mkRetVal s = (M.insert (Addr sid sqn) (NotVisited bs deps s) er, cur, gcm)
                      in case mbTxid of
                           Nothing -> mkRetVal S.empty
                           Just txid -> case M.lookup txid procTransMap of
                                          Nothing -> mkRetVal S.empty
                                          Just txnDeps -> mkRetVal txnDeps
                  GCMarker ->
                    case gcm of
                      Nothing -> (er, buildCursorFromDeps cursor ot k sid sqn deps, Just (sid, sqn, deps))
                      Just _ -> error "Multiple GC Markers") (M.empty, M.empty, Nothing) rowList
         in (M.insert (ot,k) er erm,
             M.insert (ot,k) cur curm,
             M.insert (ot,k) gcm gcmm)) (M.empty, M.empty, M.empty) rowsMap
  let filteredMap = filterUnresolved cursorMap effRowsMap
  mapM_ (\((ot,k), filteredSet) ->
    updateCache ot k filteredSet
      (fromJust $ M.lookup (ot,k) cursorMap)
      (fromJust $ M.lookup (ot, k) gcMarkerMap)) $ M.toList filteredMap
  where
    buildCursorFromDeps cursor ot k sid sqn deps =
      let gcCursor = S.foldl (\m (Addr sid sqn) -> M.insert sid sqn m)
                             (M.singleton sid sqn) deps
      in case M.lookup (ot, k) cursor of
           Nothing -> gcCursor
           Just m -> mergeCursorsAtKey m gcCursor
    isCovered cursor ot k sid sqn =
      case M.lookup (ot,k) cursor of
        Nothing -> False
        Just cursorAtKey ->
          case M.lookup sid cursorAtKey of
            Nothing -> False
            Just curSqn -> sqn <= curSqn
    updateCache ot k filteredSet gcCursor gcMarker = do
      -- Obtain locks; order is important
      cache <- takeMVar $ cm^.cacheMVar
      cursor <- takeMVar $ cm^.cursorMVar
      deps <- takeMVar $ cm^.depsMVar
      -- Update cache and (maybe) lastGCAddr
      lastGCAddr <- takeMVar $ cm^.lastGCAddrMVar
      cache <- if (newGCHasOccurred lastGCAddr gcMarker)
              then do
                let (newGCSessUUID, _, _) = fromJust $ gcMarker
                putMVar (cm^.lastGCAddrMVar) (Just newGCSessUUID)
                return $ M.insert (ot,k) S.empty cache
              else do
                putMVar (cm^.lastGCAddrMVar) lastGCAddr
                return cache
      let newCache = M.unionWith S.union cache $ M.singleton (ot,k) filteredSet
      putMVar (cm^.cacheMVar) newCache
      -- Update cursor
      let cursorAtKey = case M.lookup (ot, k) cursor of
                          Nothing -> gcCursor
                          Just m -> mergeCursorsAtKey m gcCursor
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
    newGCHasOccurred fromCache fromDB =
      case (fromCache, fromDB) of
        (Nothing, Nothing) -> False
        (Just _, Nothing) -> error "check for new GC"
        (Nothing, Just _) -> True
        (Just a, Just (sid,_,_)) ->  a /= sid



filterUnresolved :: CursorMap
                 -> M.Map (ObjType, Key) (M.Map Addr VisitedState)
                 -> M.Map (ObjType, Key) (S.Set (Addr, Effect))
filterUnresolved cm vs =
  let core = mapM_ (\((ot,k), vsObj) ->
               mapM_ (\(Addr sid sqn, _) ->
                 resolve ot k sid sqn) $ M.toList vsObj) $ M.toList vs
      ResolutionState _ vs = execState core (ResolutionState cm vs)
  in M.map (M.foldlWithKey (\s addr vs ->
       case vs of
         Visited (Just eff) -> S.insert (addr, eff) s
         otherwise -> s) S.empty) vs

resolve :: ObjType -> Key -> SessUUID -> SeqNo -> State ResolutionState Bool
resolve ot k sid sqn = do
  vs <- lookupVisitedState ot k sid sqn
  case vs of
    Visited Nothing -> return False
    Visited (Just _) -> return True
    Visiting -> return True
    NotVisited eff deps txnDeps -> do
      -- First mark this node as visiting
      updateVisitedState ot k sid sqn Visiting
      -- Process local dependences
      res1 <- foldM (\acc (Addr sid sqn) -> resolve ot k sid sqn >>= return . ((&&) acc)) True $ S.toList deps
      -- Process remote dependences
      res2 <- foldM (\acc (TxnDep ot k sid sqn) -> resolve ot k sid sqn >>= return . ((&&) acc)) res1 $ S.toList txnDeps
      -- Update final state
      if res2
      then updateVisitedState ot k sid sqn (Visited $ Just eff)
      else updateVisitedState ot k sid sqn (Visited Nothing)
      return res2
  where
    lookupVisitedState ot k sid sqn | sqn == 0 = return $ Visited (Just Data.ByteString.empty)
    lookupVisitedState ot k sid sqn | sqn > 0 = do
      vs <- use visitedState
      case M.lookup (ot,k) vs of
       {- Since the element does not exist, treat it as visited but is not part of the causal cut -}
        Nothing -> return $ Visited Nothing
        Just vsObj -> case M.lookup (Addr sid sqn) vsObj of
                        Nothing -> return $ Visited Nothing
                        Just val -> return $ val
    updateVisitedState ot k sid sqn val = do
      vs <- use visitedState
      let newVsObj = case M.lookup (ot,k) vs of
            Nothing -> M.insert (Addr sid sqn) val M.empty
            Just vsObj -> M.insert (Addr sid sqn) val vsObj
      visitedState .= M.insert (ot,k) newVsObj vs


-- Combines curors at a particular key. Since a cursor at a given (key, sessid)
-- records the largest sequence number seen so far, given two cursors at some
-- key k, the merge operation picks the larger of the sequence numbers for each
-- sessid.
mergeCursorsAtKey :: CursorAtKey -> CursorAtKey -> CursorAtKey
mergeCursorsAtKey = M.unionWith max

collectTransitiveRows :: Pool -> Consistency -> StateT CollectRowsState IO ()
collectTransitiveRows pool const = do
  to <- use todoObjs
  case S.minView to of
    Nothing -> return ()
    Just (x@(ot,k), xs) -> do
      -- Update todo list
      todoObjs .= xs
      rm <- use rowsMap
      case M.lookup x rm of
        Just _ -> collectTransitiveRows pool const
        Nothing -> do -- Work
          -- Read this (ot,k)
          rows <- liftIO $ runCas pool $ cqlRead ot const k
          -- Mark as read
          rowsMap .= M.insert (ot,k) rows rm
          mapM_ processRow rows
  where
    processRow (_,_,_,_,Nothing) = return ()
    processRow (sid, sqn, deps, val, Just txid) = do
      -- TODO: When to mark txid as done?
      includedTxns <- use includedTxnsCRS
      -- Is this transaction id already included in the cache?
      when (not $ S.member txid includedTxns) $ do
        procTxns <- use processedTxns
        -- Is this transaction already seen in this fetchUpdate?
        when (not $ M.member txid procTxns) $ do
          maybeDeps <- liftIO $ runCas pool $ readTxn txid
          case maybeDeps of
            Nothing -> -- Eventual consistency!
              return ()
            Just deps -> do
              processedTxns .= M.insert txid deps procTxns
              mapM_ processDep $ S.toList deps
    processDep dep = do
      res <- isCovered dep
      if res then return ()
      else do
        let TxnDep ot k sid sqn = dep
        to <- use todoObjs
        todoObjs .= S.insert (ot, k) to
    isCovered (TxnDep ot k sid sqn) = do
      cursor <- use cursorCRS
      case M.lookup (ot,k) cursor of
        Nothing -> return False
        Just cursorAtKey ->
          case M.lookup sid cursorAtKey of
            Nothing -> return False
            Just curSqn -> return $ (<=) sqn curSqn
