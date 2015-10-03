{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Quelea.Client (
  Key,
  Session,
  Availability(..),
  TxnKind(..),

  beginSession,
  beginSessionStats,
  getStats,
  endSession,
  invoke,
  invokeAndGetDeps,
  newKey,
  mkKey,
  getServerAddr,
  beginTxn,
  endTxn,
  getLastEffect
) where

import Data.UUID
import Quelea.Types
import Quelea.NameService.Types
import Control.Lens
import Control.Monad (when)
import System.ZMQ4
import Data.Serialize
import Quelea.Marshall
import System.Random (randomIO)
import Control.Applicative
import Data.ByteString (ByteString, length)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.Tuple.Select
import Debug.Trace
import Data.Time

type Effect = ByteString


data TxnState = TxnState {
  _txnidTS    :: TxnID,
  _txnKindTS  :: TxnKind,
  _txnEffsTS  :: S.Set TxnDep,
  _snapshotTS :: M.Map (ObjType, Key) (S.Set (Addr, Effect)),
  _seenTxnsTS :: S.Set TxnID
} deriving Eq

makeLenses ''TxnState

data Session = Session {
  _broker     :: Frontend,
  _server     :: Socket Req,
  _serverAddr :: String,
  _sessid     :: SessID,
  _seqMap     :: M.Map (ObjType, Key) SeqNo,
  _readObjs   :: S.Set (ObjType, Key),

  _collectStats :: Bool,
  _numOps       :: Int,
  _startTime    :: UTCTime,
  _avgLat       :: NominalDiffTime,

  _curTxn     :: Maybe TxnState,
  _lastEffect :: Maybe TxnDep

}

makeLenses ''Session

getStats :: Session -> IO (Double, NominalDiffTime)
getStats s =
  if s^.collectStats
  then do
    now <- getCurrentTime
    let diffTime = diffUTCTime now $ s^.startTime
    let d::Double = realToFrac $ (fromIntegral $ s^.numOps) / diffTime
    return (d, s^.avgLat)
  else return $ (0.0,0)

beginSessionStats :: NameService -> Bool -> IO Session
beginSessionStats ns getStats = do
  (serverAddr, sock) <- getClientJoin ns
  -- Create a session id
  sessid <- SessID <$> randomIO
  currentTime <- getCurrentTime
  -- Initialize session
  return $ Session (getFrontend ns) sock serverAddr sessid M.empty S.empty getStats 0 currentTime 0 Nothing Nothing

beginSession :: NameService -> IO Session
beginSession ns = beginSessionStats ns False

endSession :: Session -> IO ()
endSession s = disconnect (s ^. server) (s^.serverAddr)

beginTxn :: Session -> TxnKind -> IO Session
beginTxn s tk = do
  when (s^.curTxn /= Nothing) $ error "beginTxn: Nested transactions are not supported!"
  txnID <- TxnID <$> randomIO
  snapshot <-
    if (tk == RR)
    then do
      let req :: Request () = ReqSnapshot $ s^.readObjs
      let encodedReq = encode req
      -- putStrLn $ "beginTxn: req length=" ++ show (B.length encodedReq)
      send (s^.server) [] encodedReq
      responseBlob <- receive (s^.server)
      -- putStrLn $ "beginTxn: res length=" ++ show (B.length responseBlob)
      let (ResSnapshot s) = decodeResponse responseBlob
      return $ Just s
    else return Nothing
  return $ s {_curTxn = Just $ TxnState txnID tk S.empty M.empty S.empty}

endTxn :: S.Set TxnDep {- Extra dependencies -} -> Session -> IO Session
endTxn extraDeps s = do
  when (s^.curTxn == Nothing) $ error "endTxn: No transaction in progress!"
  let txnState = case s^.curTxn of {Nothing -> error "endTxn"; Just st -> st}
  let txnCommit :: Request () = ReqTxnCommit (txnState^.txnidTS) (S.union extraDeps $ txnState^.txnEffsTS)
  send (s^.server) [] $ encode txnCommit
  receive (s^.server)
  return $ s {_curTxn = Nothing}

getServerAddr :: Session -> String
getServerAddr s = s^.serverAddr

invoke :: (OperationClass on, Serialize arg, Serialize res)
       => Session -> Key -> on -> arg -> IO (res, Session)
invoke s k on arg = do
  startTime <- getCurrentTime
  (r, deps, s) <- invokeInternal False s k on arg
  endTime <- getCurrentTime
  let newOpsCnt = s^.numOps + 1
  let newAvgLat = (s^.avgLat * (fromIntegral $ s^.numOps) + (diffUTCTime endTime startTime)) / (fromIntegral $ s^.numOps + 1)
  return (r,s {_numOps = newOpsCnt, _avgLat = newAvgLat})

invokeAndGetDeps :: (OperationClass on, Serialize arg, Serialize res)
       => Session -> Key -> on -> arg -> IO (res, S.Set TxnDep, Session)
invokeAndGetDeps = invokeInternal True

invokeInternal :: (OperationClass on, Serialize arg, Serialize res)
       => Bool -> Session -> Key -> on -> arg -> IO (res, S.Set TxnDep, Session)
invokeInternal getDeps s key operName arg = do
  let ot = getObjType operName
  let seqNo = case M.lookup (ot, key) $ s^.seqMap of
                Nothing -> 0
                Just s -> s
  let txnReq = mkTxnReq ot key $ s^.curTxn
  let req = encode $ ReqOper $ OperationPayload ot key operName (encode arg)
                        (s ^. sessid) seqNo txnReq getDeps
  -- putStrLn $ "invokeInternal: req length=" ++ show (B.length req)
  send (s^.server) [] req
  responseBlob <- receive (s^.server)
  -- putStrLn $ "invokedInternal: res length=" ++ show (B.length responseBlob)
  let (ResOper newSeqNo resBlob mbNewEff mbTxns visAddrSet) = decodeResponse responseBlob
  let visSet = S.map (\(Addr sid sqn) -> TxnDep ot key sid sqn) visAddrSet
  case decode resBlob of
    Left s -> error $ "invoke : decode failure " ++ s
    Right res -> do
      let newSeqMap = M.insert (ot, key) newSeqNo $ s^.seqMap
      {- XXX KC -}
      let newReadObjs = if (S.size $ s^.readObjs) > 32
                        then S.insert (ot,key) $ S.deleteMin $ s^.readObjs
                        else S.insert (ot,key) $ s^.readObjs
      let partialSessRV = Session (s^.broker) (s^.server) (s^.serverAddr) (s^.sessid) newSeqMap newReadObjs
                                  (s^.collectStats) (s^.numOps) (s^.startTime) (s^.avgLat)
      let newLastEff = Just $ TxnDep ot key (s^.sessid) newSeqNo
      case s^.curTxn of
        Nothing -> {- This operation was not in a transaction -}
          if newSeqNo == seqNo {- This operation was read only -}
          then return (res, visSet, partialSessRV Nothing Nothing)
          else return (res, visSet, partialSessRV Nothing newLastEff)
        Just orig@(TxnState txid txnKind deps cache seenTxns) -> do
          case mbNewEff of
            Nothing -> {- This operation was read only -}
              return (res, visSet, partialSessRV (Just orig) Nothing)
            Just newEff -> do
              let newDeps = S.insert (TxnDep ot key (s^.sessid) newSeqNo) deps
              let (_, txnPayload) = case txnReq of {Nothing -> error "invokeInternal"; Just x -> x}
              let newSeenTxns = case mbTxns of
                              Nothing -> seenTxns
                              Just ts -> S.union ts seenTxns
              let newCache =
                    case getEffectSet txnPayload of
                      Nothing -> cache
                      Just es -> M.insert (ot, key) (S.insert (Addr (s^.sessid) newSeqNo, newEff) es) cache
              let newTxnState = (Just (TxnState txid txnKind newDeps newCache newSeenTxns))
              return (res, visSet, partialSessRV newTxnState newLastEff)
  where
    getEffectSet (RC_TxnPl es) = Just es
    getEffectSet (MAV_TxnPl es _) = Just es
    getEffectSet (RR_TxnPl es) = Just es

    mkTxnReq ot k Nothing = Nothing
    mkTxnReq ot k (Just ts) =
      let cache = ts^.snapshotTS
          txid = ts^.txnidTS
          es = case M.lookup (ot,k) cache of
                 Nothing -> S.empty
                 Just s -> s
      in Just $ case ts^.txnKindTS of
           RC ->
             (txid, RC_TxnPl es)
           RR ->
             (txid, RR_TxnPl es)
           MAV -> (txid, MAV_TxnPl es $ ts^.seenTxnsTS)

newKey :: IO Key
newKey = Key . encodeUUID <$> randomIO
  where
    encodeUUID (uuid :: UUID) = encode uuid

mkKey :: Serialize a => a -> Key
mkKey kv = Key $ encode kv

getLastEffect :: Session -> Maybe TxnDep
getLastEffect s = s^.lastEffect
