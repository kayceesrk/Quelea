{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell, DataKinds, OverloadedStrings, DoAndIfThenElse  #-}

module Codeec.ShimLayer.Cache (
  Cache,

  initCache,
  getContext,
  addEffectToCache,
  getHistory,
  batchUpdate
) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString hiding (map, pack, putStrLn)
import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Codeec.Types
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.State
import Data.Map.Lens
import Control.Monad (forever)
import Data.Maybe (fromJust)

type History = M.Map (ObjType, Key, SessUUID) SeqNo
type Effect = ByteString

data ReqMsgs =
    GetCtxt ObjType Key
  | AddEffect ObjType Key SessUUID SeqNo Effect
  | GetHistory
  | BatchUpdate (M.Map (ObjType, Key, SessUUID, SeqNo) Effect)

data ResMsgs =
    Ctxt [Effect]
  | Hist History

data Cache = Cache (MVar ReqMsgs) (MVar ResMsgs)

data CacheState = CacheState {
  _cache   :: M.Map (ObjType, Key) [Effect],
  _history :: History,
  -- interveningWrite == Nothing if batch update is not in progress
  _interveningWrites :: Maybe (S.Set (ObjType, Key, SessUUID, SeqNo))
}

type CacheMonad a = StateT CacheState IO a

makeLenses ''CacheState

cacheCore :: Cache -> CacheMonad ()
cacheCore (Cache reqMVar resMVar) = forever $ do
  request <- liftIO $ takeMVar reqMVar
  case request of
    GetCtxt objType key -> do
      c <- use cache;
      let ctxt = fromJust $ c ^.at (objType, key)
      liftIO $ putMVar resMVar $ Ctxt ctxt
    GetHistory -> do
      h <- use history
      liftIO $ putMVar resMVar $ Hist h
      interveningWrites .= Just S.empty
    AddEffect objType key sessid seqno eff -> do
      addEffect (objType, key, sessid, seqno) eff
    BatchUpdate updateMap -> do
      iv <- use interveningWrites
      let updateMapFiltered =
            case iv of
              Nothing -> updateMap
              Just ivSet -> S.foldl (\m e -> M.delete e m) updateMap ivSet
      let updateList = M.toList updateMapFiltered
      interveningWrites .= Nothing
      {- The following invocation Only works if the effects are added in seq
       - number order(1), without missing any intervening effects(2). (1)
       - should be ensured since the quadruple (objType, key, sessid, seqno) is
       - derived from the key of the update map. (2) is taken care of by the
       - update fetcher daemon. TODO: Optimize. -}
      mapM_ (\(k,v) -> addEffect k v) updateList
  where
    addEffect (objType, key, sessid, seqno) eff = do
        h <- use history
        if (seqno == 1 || h ^.at (objType, key, sessid) == (Just $ seqno - 1))
        then do
          -- update history
          history .= (at (objType, key, sessid) .~ Just seqno $ h)
          -- update cache
          c <- use cache
          let ctxt = (\x -> case x of {Nothing -> []; Just l -> l}) (c ^.at (objType, key))
          cache .= (at (objType, key) .~ Just (eff:ctxt) $ c)
          -- record intervening write if necessary
          iv <- use interveningWrites
          case iv of
            Nothing -> return ()
            Just set -> do
              let newSet = S.insert (objType, key, sessid, seqno) set
              interveningWrites .= Just newSet
        else return ()

initCache :: IO Cache
initCache = undefined

addEffectToCache :: Cache -> ObjType -> Key -> SessUUID -> SeqNo -> Effect -> IO Bool
addEffectToCache = undefined

getContext :: Cache -> ObjType -> Key -> IO [Effect]
getContext = undefined

getHistory :: Int
getHistory = undefined

batchUpdate :: Int
batchUpdate = undefined
