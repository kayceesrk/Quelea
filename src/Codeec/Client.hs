{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Codeec.Client (
  Key,
  Session,

  beginSession,
  endSession,
  invoke,
  newKey,
  mkKey,
  getUUID,
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

type Effect = ByteString

data Session = Session {
  _broker     :: Frontend,
  _server     :: Socket Req,
  _serverAddr :: String,
  _sessid     :: SessID,
  _seqMap     :: M.Map (ObjType, Key) SeqNo,
  _curTxn     :: Maybe (TxnID, S.Set TxnDep, M.Map (ObjType, Key) (S.Set (Addr, Effect)))
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
  return $ Session fe sock serverAddr sessid M.empty Nothing

endSession :: Session -> IO ()
endSession s = disconnect (s ^. server) (s^.serverAddr)

beginTxn :: Session -> IO Session
beginTxn s = do
  when (s^.curTxn /= Nothing) $ error "beginTxn: Nested transactions are not supported!"
  txnID <- TxnID <$> randomIO
  return $ s {_curTxn = Just (txnID, S.empty, M.empty)}

endTxn :: Session -> IO Session
endTxn s = do
  when (s^.curTxn == Nothing) $ error "endTxn: No transaction in progress!"
  let (txnID, deps, _) = fromJust $ s^.curTxn
  let txnCommit :: Request () = ReqTxnCommit txnID deps
  send (s^.server) [] $ encode txnCommit
  receive (s^.server)
  return $ s {_curTxn = Nothing}

getServerAddr :: Session -> String
getServerAddr s = s^.serverAddr

invoke :: (OperationClass on, Serialize arg, Serialize res)
       => Session -> Key -> on -> arg -> IO (res, Session)
invoke s key operName arg = do
  let objType = getObjType operName
  let seqNo = case M.lookup (objType, key) $ s^.seqMap of
                Nothing -> 0
                Just s -> s
  let txnReq = mkTxnReq objType key $ s^.curTxn
  let req = ReqOper $ OperationPayload objType key operName (encode arg)
                        (s ^. sessid) seqNo txnReq
  send (s^.server) [] $ encode req
  responseBlob <- receive (s^.server)
  let (Response newSeqNo resBlob mbNewEff) = decodeResponse responseBlob
  case decode resBlob of
    Left s -> error $ "invoke : decode failure " ++ s
    Right res -> do
      let newSeqMap = M.insert (objType, key) newSeqNo $ s^.seqMap
      let partialSessRV = Session (s^.broker) (s^.server) (s^.serverAddr)
                                  (s^.sessid) newSeqMap
      case s^.curTxn of
        Nothing -> return (res, partialSessRV Nothing)
        Just (txid, deps, cache) ->
          case mbNewEff of
            Nothing -> return (res, partialSessRV (Just (txid, deps, cache)))
            Just newEff -> do
              let newDeps = S.insert (TxnDep objType key (s^.sessid) newSeqNo) deps
              let (_, es) = fromJust txnReq
              let newCache = M.insert (objType, key) (S.insert (Addr (s^.sessid) newSeqNo, newEff) es) cache
              return (res, partialSessRV (Just (txid, newDeps, newCache)))
  where
    mkTxnReq ot k Nothing = Nothing
    mkTxnReq ot k (Just (txid, _, cache)) =
      Just (txid, case M.lookup (ot,k) cache of {Nothing -> S.empty; Just el -> el})

newKey :: IO Key
newKey = Key <$> randomIO

mkKey :: UUID -> Key
mkKey uuid = Key uuid

getUUID :: Key -> UUID
getUUID (Key k) = k
