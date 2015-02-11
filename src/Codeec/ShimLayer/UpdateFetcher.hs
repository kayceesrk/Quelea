{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DoAndIfThenElse, BangPatterns  #-}

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
import Control.Concurrent (myThreadId)
import Debug.Trace
import System.IO (hFlush, stdout)
import System.Posix.Process (getProcessID)
import Data.Tuple.Select
import Data.Time

import Codeec.Consts
import Codeec.Types
import Codeec.ShimLayer.Types
import Codeec.DBDriver

makeLenses ''CacheManager
makeLenses ''Addr

data CollectRowsState = CollectRowsState {
  _inclTxnsCRS :: S.Set TxnID,
  _todoObjsCRS :: S.Set (ObjType, Key),
  _rowsMapCRS  :: M.Map (ObjType, Key) [ReadRow],
  _newTxnsCRS  :: M.Map TxnID (S.Set TxnDep)
}

makeLenses ''CollectRowsState
makeLenses ''TxnDep

data VisitedState = Visited (Maybe (Effect, Maybe TxnID))
                  | NotVisited { effect  :: Effect,
                                 deps    :: S.Set Addr,
                                 txnid   :: Maybe TxnID,
                                 txndeps :: S.Set TxnDep }
                  | Visiting deriving Show

data ResolutionState = ResolutionState {
  _visitedState :: M.Map (ObjType, Key) (M.Map Addr VisitedState)
}

makeLenses ''ResolutionState

#define DEBUG

debugPrint :: String -> IO ()
#ifdef DEBUG
debugPrint s = do
  tid <- myThreadId
  pid <- getProcessID
  putStrLn $ "[" ++ (show pid) ++ "," ++ (show tid) ++ "] " ++ s
  hFlush stdout
#else
debugPrint _ = return ()
#endif

data CacheUpdateState = CacheUpdateState {
  _cacheCUS      :: CacheMap,
  _cursorCUS     :: CursorMap,
  _depsCUS       :: NearestDepsMap,
  _lastGCAddrCUS :: M.Map (ObjType, Key) SessID,
  _lastGCTimeCUS :: M.Map (ObjType, Key) UTCTime,
  _inclTxnsCUS   :: (S.Set TxnID, M.Map (ObjType, Key) (S.Set TxnID))
}

makeLenses ''CacheUpdateState

fetchUpdates :: CacheManager -> Consistency -> [(ObjType, Key)] -> IO ()
fetchUpdates cm const todoList = do
  -- Recursively read the DB and collect all the rows
  !cursor <- readMVar $ cm^.cursorMVar
  !inclTxns <- readMVar $ cm^.includedTxnsMVar
  !lgctMap <- readMVar $ cm^.lastGCTimeMVar

  let todoObjsCRS = S.fromList todoList
  let crs = CollectRowsState (sel1 inclTxns) todoObjsCRS M.empty M.empty
  CollectRowsState _ _ !rowsMapCRS !newTransMap <- execStateT (collectTransitiveRows (cm^.pool) const lgctMap) crs

  -- Update disk row count for later use by gcDB
  drc <- takeMVar $ cm^.diskRowCntMVar
  let newDrc = M.foldlWithKey (\iDrc (ot,k) rows -> M.insert (ot,k) (length rows) iDrc) drc rowsMapCRS
  putMVar (cm^.diskRowCntMVar) newDrc

  -- First collect the GC markers
  let gcMarkerMap = M.foldlWithKey (\gcmm (ot,k) rowList ->
         let gcm = foldl (\gcm (sid::SessID,sqn,deps,val,mbTxid) ->
                case val of
                  EffectVal bs -> gcm
                  GCMarker atTime ->
                    case gcm of
                      Nothing -> Just (sid, sqn, deps, atTime)
                      Just _ -> error "Multiple GC Markers") Nothing rowList
         in M.insert (ot,k) gcm gcmm) M.empty rowsMapCRS

  -- Build new cursor with GC markers. The idea here is that if we did find a
  -- GC marker & we have not seen this marker already, then keep hold of all
  -- the rows except the ones explicitly covered by the GC. The reason is that
  -- subsequently, we will clear our cache and rebuild it. Here, it is not
  -- correct to ignore those rows, which might have already been seen, but was
  -- not GCed.
  !lastGCAddrMap <- readMVar $ cm^.lastGCAddrMVar
  let !newCursor = M.foldlWithKey (\c (ot,k) gcMarker ->
                    let lastGCAddr = M.lookup (ot,k) lastGCAddrMap
                    in if newGCHasOccurred lastGCAddr gcMarker
                         then case gcMarker of
                                Nothing -> c
                                Just marker ->
                                  let gcCursor = buildCursorFromGCMarker marker
                                  in M.insert (ot,k) gcCursor c
                         else c)
                     cursor gcMarkerMap

  -- Once we have built the cursor, filter those rows which are covered.
  let effRowsMap = M.foldlWithKey (\erm (ot,k) rowList ->
         let er = foldl (\er (sid,sqn,deps,val,mbTxid) ->
                case val of
                  EffectVal bs ->
                    if isCovered newCursor ot k sid sqn
                    then er
                    else
                      let mkRetVal txid txdeps = M.insert (Addr sid sqn) (NotVisited bs deps txid txdeps) er
                      in case mbTxid of
                           Nothing -> mkRetVal Nothing S.empty
                           Just txid -> case M.lookup txid newTransMap of
                                          Nothing -> mkRetVal (Just txid) S.empty {- Eventual consistency (or) txn in progress! -}
                                          Just txnDeps -> mkRetVal (Just txid) txnDeps
                  GCMarker _ -> er) M.empty rowList
         in M.insert (ot,k) er erm) M.empty rowsMapCRS

  -- debugPrint $ "newCursor"
  -- mapM_ (\((ot,k), m) -> mapM_ (\(sid,sqn) -> debugPrint $ show $ Addr sid sqn) $ M.toList m) $ M.toList newCursor

  -- Now filter those rows which are unresolved i.e) those rows whose
  -- dependencies are not visible.
  let !filteredMap = filterUnresolved newCursor effRowsMap

  -- debugPrint $ "filteredMap"
  -- mapM_ (\((ot,k), s) -> mapM_ (\(addr,_,_) -> debugPrint $ show addr) $ S.toList s) $ M.toList filteredMap

  -- Update state. First obtain locks...
  !cache      <- takeMVar $ cm^.cacheMVar
  !cursor     <- takeMVar $ cm^.cursorMVar
  !deps       <- takeMVar $ cm^.depsMVar
  !lastGCAddr <- takeMVar $ cm^.lastGCAddrMVar
  !lastGCTime <- takeMVar $ cm^.lastGCTimeMVar
  !inclTxns   <- takeMVar $ cm^.includedTxnsMVar

  let core =
        mapM_ (\((ot,k), filteredSet) ->
          updateCache ot k filteredSet
            (case M.lookup (ot,k) newCursor of {Nothing -> M.empty; Just m -> m})
            (fromJust $ M.lookup (ot,k) gcMarkerMap)) $ M.toList filteredMap

  let CacheUpdateState !newCache !new2Cursor !newDeps !newLastGCAddr !newLastGCTime !newInclTxns =
        execState core (CacheUpdateState cache cursor deps lastGCAddr lastGCTime inclTxns)

  -- debugPrint $ "finalCursor"
  -- mapM_ (\((ot,k), m) -> mapM_ (\(sid,sqn) -> debugPrint $ show $ Addr sid sqn) $ M.toList m) $ M.toList new2Cursor

  -- Flush cache if necessary {- Catch: XXX KC: A query cannot desire to see more than 1024 objects -}
  if M.size newCache < cCACHE_MAX_OBJS
  then do
    putMVar (cm^.cacheMVar) newCache
    putMVar (cm^.cursorMVar) new2Cursor
    putMVar (cm^.depsMVar) newDeps
    putMVar (cm^.lastGCAddrMVar) newLastGCAddr
    putMVar (cm^.lastGCTimeMVar) newLastGCTime
    putMVar (cm^.includedTxnsMVar) newInclTxns
  else do
    -- Reset almost everything
    putMVar (cm^.cacheMVar) M.empty
    putMVar (cm^.cursorMVar) M.empty
    putMVar (cm^.depsMVar) M.empty
    putMVar (cm^.lastGCAddrMVar) M.empty
    putMVar (cm^.lastGCTimeMVar) M.empty
    putMVar (cm^.includedTxnsMVar) (S.empty, M.empty)

    takeMVar (cm^.hwmMVar)
    putMVar (cm^.hwmMVar) M.empty
    takeMVar (cm^.diskRowCntMVar)
    putMVar (cm^.diskRowCntMVar) M.empty
    takeMVar (cm^.hotLocsMVar)
    putMVar (cm^.hotLocsMVar) S.empty

    -- fetch updates again
    fetchUpdates cm const todoList
  where
    buildCursorFromGCMarker (sid, sqn, deps,_) =
      S.foldl (\m (Addr sid sqn) ->
            case M.lookup sid m of
              Nothing -> M.insert sid sqn m
              Just oldSqn -> M.insert sid (max oldSqn sqn) m) (M.singleton sid sqn) deps
    updateCache ot k filteredSet gcCursor gcMarker = do
      -- Handle GC
      lgca <- use lastGCAddrCUS
      let cacheGCId = M.lookup (ot,k) lgca
      -- If a new GC has occurred, flush the cache and get the new effects
      -- inserted by the GC.
      when (newGCHasOccurred cacheGCId gcMarker) $ do
        -- Update GC information
        let (newGCSessID, _, _, atTime) = fromJust $ gcMarker
        lgca <- use lastGCAddrCUS
        lastGCAddrCUS .= M.insert (ot,k) newGCSessID lgca
        lgct <- use lastGCTimeCUS
        lastGCTimeCUS .= M.insert (ot,k) atTime lgct
        -- empty cache
        cache <- use cacheCUS
        cacheCUS .= M.insert (ot,k) S.empty cache
        -- reset cursor
        cursor <- use cursorCUS
        cursorCUS .= M.insert (ot,k) M.empty cursor
        -- reset deps
        deps <- use depsCUS
        depsCUS .= M.insert (ot,k) S.empty deps

      -- Update cache
      cache <- use cacheCUS
      let newEffs :: CacheMap = M.singleton (ot,k) (S.map (\(a,e,_) -> (a,e)) filteredSet)
      cacheCUS .= M.unionWith S.union cache newEffs
      -- Update cursor
      cursor <- use cursorCUS
      let cursorAtKey = case M.lookup (ot, k) cursor of
                          Nothing -> gcCursor
                          Just m -> mergeCursorsAtKey m gcCursor
      let newCursorAtKey = S.foldl (\m (Addr sid sqn, _, _) ->
                              case M.lookup sid m of
                                Nothing -> M.insert sid sqn m
                                Just oldSqn -> if oldSqn < sqn
                                              then M.insert sid sqn m
                                              else m) cursorAtKey filteredSet
      cursorCUS .= M.insert (ot,k) newCursorAtKey cursor

      -- Update dependence
      deps <- use depsCUS
      let curDepsMap = case M.lookup (ot,k) deps of
                         Nothing -> M.empty
                         Just s -> S.foldl (\m (Addr sid sqn) ->
                                     case M.lookup sid m of
                                       Nothing -> M.insert sid sqn m
                                       Just oldSqn -> if oldSqn < sqn
                                                      then M.insert sid sqn m
                                                      else m) M.empty s
      let maxSqnMap = S.foldl (\m (Addr sid sqn,_,_) ->
                                  case M.lookup sid m of
                                    Nothing -> M.insert sid sqn m
                                    Just oldSqn -> if oldSqn < sqn
                                                   then M.insert sid sqn m
                                                   else m) curDepsMap filteredSet
      -- Just convert "Map sid sqn" to "Set (Addr sid sqn)"
      let maxSqnSet = M.foldlWithKey (\s sid sqn -> S.insert (Addr sid sqn) s) S.empty maxSqnMap
      -- Insert into deps to create newDeps
      -- OLD CODE : let newDeps = M.unionWith S.union deps $ M.singleton (ot,k) maxSqnSet
      let newDeps = M.insert (ot,k) maxSqnSet deps
      depsCUS .= newDeps

      -- Update included transactions
      (inclTxnsSet, inclTxnsMap) <- use inclTxnsCUS
      let newTxns = S.foldl (\acc (_,_,mbTxid) ->
                        case mbTxid of
                          Nothing -> acc
                          Just txid -> S.insert txid acc) S.empty filteredSet
      let newInclTxns = (S.union inclTxnsSet newTxns, M.insertWith S.union (ot,k) newTxns inclTxnsMap)
      inclTxnsCUS .= newInclTxns

    newGCHasOccurred :: Maybe SessID -> Maybe (SessID, SeqNo, S.Set Addr, UTCTime) -> Bool
    newGCHasOccurred Nothing Nothing = False
    newGCHasOccurred Nothing (Just _) = True
    newGCHasOccurred (Just _) Nothing = error "newGCHasOccurred: unexpected state"
    newGCHasOccurred (Just fromCache) (Just (fromDB,_,_,_)) = fromCache /= fromDB


isCovered :: CursorMap -> ObjType -> Key -> SessID -> SeqNo -> Bool
isCovered cursor ot k sid sqn =
  case M.lookup (ot,k) cursor of
    Nothing -> False
    Just cursorAtKey ->
      case M.lookup sid cursorAtKey of
        Nothing -> False
        Just curSqn -> sqn <= curSqn

filterUnresolved :: CursorMap
                 -> M.Map (ObjType, Key) (M.Map Addr VisitedState)
                 -> M.Map (ObjType, Key) (S.Set (Addr, Effect, Maybe TxnID))
filterUnresolved cm vs1 =
  let core = mapM_ (\((ot,k), vsObj) ->
               mapM_ (\(Addr sid sqn, _) ->
                 resolve cm ot k sid sqn) $ M.toList vsObj) $ M.toList vs1
      ResolutionState vs2 = execState core (ResolutionState vs1)
  in M.map (M.foldlWithKey (\s addr vs ->
       case vs of
         Visited (Just (eff, mbTxid)) -> S.insert (addr, eff, mbTxid) s
         otherwise -> s) S.empty) vs2

resolve :: CursorMap -> ObjType -> Key -> SessID -> SeqNo -> State ResolutionState Bool
resolve cursor ot k sid sqn = do
  vs <- lookupVisitedState cursor ot k sid sqn
  case vs of
    Visited Nothing -> return False
    Visited (Just _) -> return True
    Visiting -> return True
    NotVisited eff deps mbTxid txnDeps -> do
      -- First mark this node as visiting
      updateVisitedState ot k sid sqn Visiting
      -- Process local dependences
      res1 <- foldM (\acc (Addr sid sqn) ->
                resolve cursor ot k sid sqn >>= return . ((&&) acc)) True $ S.toList deps
      -- Process remote dependences
      res2 <- case mbTxid of
        Nothing -> return res1
        Just txid ->
          if S.size txnDeps == 0
          then return False {- Txn in progress (or) eventual consistency -}
          else foldM (\acc (TxnDep ot k sid sqn) ->
                 resolve cursor ot k sid sqn >>= return . ((&&) acc)) res1 $ S.toList txnDeps
      -- Update final state
      if res2
      then updateVisitedState ot k sid sqn (Visited $ Just (eff, mbTxid))
      else updateVisitedState ot k sid sqn (Visited Nothing)
      return res2
  where
    trueVal = Visiting
    falseVal = Visited Nothing
    lookupVisitedState cursor ot k sid sqn | sqn == 0 =
      return trueVal
    lookupVisitedState cursor ot k sid sqn | sqn > 0 = do
      if isCovered cursor ot k sid sqn
      then return trueVal
      else do
        vs <- use visitedState
        case M.lookup (ot,k) vs of
          Nothing -> return falseVal
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

collectTransitiveRows :: Pool -> Consistency
                      -> M.Map (ObjType, Key) UTCTime
                      -> StateT CollectRowsState IO ()
collectTransitiveRows pool const lgct = do
  to <- use todoObjsCRS
  case S.minView to of
    Nothing -> return ()
    Just (x@(ot,k), xs) -> do
      -- Update todo list
      todoObjsCRS .= xs
      rm <- use rowsMapCRS
      case M.lookup x rm of
        Just _ -> return ()
        Nothing -> do -- Work
          -- Read this (ot,k)
          rows <- case M.lookup (ot,k) lgct of
                  Nothing -> liftIO $ do
                    runCas pool $ cqlRead ot const k
                  Just gcTime -> liftIO $ do
                    runCas pool $ cqlReadAfterTime ot const k gcTime
          -- Mark as read
          rowsMapCRS .= M.insert (ot,k) rows rm
          mapM_ processRow rows
      collectTransitiveRows pool const lgct
  where
    processRow (_,_,_,_,Nothing) = return ()
    processRow (sid, sqn, deps, val, Just txid) = do
      includedTxns <- use inclTxnsCRS
      -- Is this transaction id already included in the cache?
      when (not $ S.member txid includedTxns) $ do
        procTxns <- use newTxnsCRS
        -- Is this transaction already seen in this fetchUpdate?
        when (not $ M.member txid procTxns) $ do
          maybeDeps <- liftIO $ runCas pool $ readTxn txid
          case maybeDeps of
            Nothing -> -- Eventual consistency!
              return ()
            Just deps -> do
              newTxnsCRS .= M.insert txid deps procTxns
              mapM_ maybeAddObjToTodo $ S.toList deps
    maybeAddObjToTodo dep = do
      to <- use todoObjsCRS
      rm <- use rowsMapCRS
      let TxnDep ot k sid sqn = dep
      if S.member (ot,k) to || M.member (ot,k) rm
      then return ()
      else do
        todoObjsCRS .= S.insert (ot, k) to
