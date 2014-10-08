{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Codeec.Client (
  Key,
  Session,
  TxnKind(..),

  beginSession,
  endSession,
  invoke,
  newKey,
  mkKey,
  getServerAddr,
  beginTxn,
  endTxn
) where

import Data.UUID
import Codeec.Types
import Codeec.NameService.SimpleBroker
import Control.Lens
import Control.Monad (when)
import System.ZMQ4
import Data.Serialize
import Codeec.Marshall
import System.Random (randomIO)
import Control.Applicative
import Data.ByteString (cons, ByteString)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.Tuple.Select
import Debug.Trace

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
  _curTxn     :: Maybe TxnState
}

makeLenses ''Session

beginSession :: Frontend -> IO Session
beginSession fe = do
  {- connect to the frontend of shim layer broker, which returns one of the
   - shim layer node addresses. -}
  serverAddr <- clientJoin fe
  -- Connect to the shim layer node.
  ctxt <- context
  sock <- socket ctxt Req
  connect sock serverAddr
  -- Create a session id
  sessid <- SessID <$> randomIO
  -- Initialize session
  return $ Session fe sock serverAddr sessid M.empty S.empty Nothing

endSession :: Session -> IO ()
endSession s = disconnect (s ^. server) (s^.serverAddr)

beginTxn :: Session -> TxnKind -> IO Session
beginTxn s tk = do
  when (s^.curTxn /= Nothing) $ error "beginTxn: Nested transactions are not supported!"
  txnID <- TxnID <$> randomIO
  snapshot <-
    if (tk == RepeatableRead)
    then do
      let req :: Request () = ReqSnapshot $ s^.readObjs
      send (s^.server) [] $ encode req
      responseBlob <- receive (s^.server)
      let (ResSnapshot s) = decodeResponse responseBlob
      return $ Just s
    else return Nothing
  return $ s {_curTxn = Just $ TxnState txnID tk S.empty M.empty S.empty}

endTxn :: Session -> IO Session
endTxn s = do
  when (s^.curTxn == Nothing) $ error "endTxn: No transaction in progress!"
  let txnState = fromJust $ s^.curTxn
  let txnCommit :: Request () = ReqTxnCommit (txnState^.txnidTS) (txnState^.txnEffsTS)
  send (s^.server) [] $ encode txnCommit
  receive (s^.server)
  return $ s {_curTxn = Nothing}

getServerAddr :: Session -> String
getServerAddr s = s^.serverAddr

invoke :: (OperationClass on, Serialize arg, Serialize res)
       => Session -> Key -> on -> arg -> IO (res, Session)
invoke s key operName arg = do
  let ot = getObjType operName
  let seqNo = case M.lookup (ot, key) $ s^.seqMap of
                Nothing -> 0
                Just s -> s
  let txnReq = mkTxnReq ot key $ s^.curTxn
  let req = ReqOper $ OperationPayload ot key operName (encode arg)
                        (s ^. sessid) seqNo txnReq
  send (s^.server) [] $ encode req
  responseBlob <- receive (s^.server)
  let (ResOper newSeqNo resBlob mbNewEff mbTxns) = decodeResponse responseBlob
  case decode resBlob of
    Left s -> error $ "invoke : decode failure " ++ s
    Right res -> do
      let newSeqMap = M.insert (ot, key) newSeqNo $ s^.seqMap
      let newReadObjs = S.insert (ot,key) $ s^.readObjs
      let partialSessRV = Session (s^.broker) (s^.server) (s^.serverAddr)
                                  (s^.sessid) newSeqMap newReadObjs
      case s^.curTxn of
        Nothing -> return (res, partialSessRV Nothing)
        Just orig@(TxnState txid txnKind deps cache seenTxns) -> do
          case mbNewEff of
            Nothing -> return (res, partialSessRV (Just orig))
            Just newEff -> do
              let newDeps = S.insert (TxnDep ot key (s^.sessid) newSeqNo) deps
              let (_, txnPayload) = fromJust txnReq
              let newSeenTxns = case mbTxns of
                              Nothing -> seenTxns
                              Just ts -> S.union ts seenTxns
              let newCache =
                    case getEffectSet txnPayload of
                      Nothing -> cache
                      Just es -> M.insert (ot, key) (S.insert (Addr (s^.sessid) newSeqNo, newEff) es) cache
              return (res, partialSessRV (Just (TxnState txid txnKind newDeps newCache newSeenTxns)))
  where
    getEffectSet (RC es) = Just es
    getEffectSet (MAV es _) = Just es
    getEffectSet (RR es) = Just es

    mkTxnReq ot k Nothing = Nothing
    mkTxnReq ot k (Just ts) =
      let cache = ts^.snapshotTS
          txid = ts^.txnidTS
          es = case M.lookup (ot,k) cache of
                 Nothing -> S.empty
                 Just s -> s
      in Just $ case ts^.txnKindTS of
           ReadCommitted ->
             (txid, RC es)
           RepeatableRead ->
             (txid, RR es)
           MonotonicAtomicView -> (txid, MAV es $ ts^.seenTxnsTS)

newKey :: IO Key
newKey = Key . encodeUUID <$> randomIO
  where
    encodeUUID (uuid :: UUID) = encode uuid

mkKey :: Serialize a => a -> Key
mkKey kv = Key $ encode kv
