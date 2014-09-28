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
import Data.ByteString (cons)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

data Session = Session {
  _broker     :: Frontend,
  _server     :: Socket Req,
  _serverAddr :: String,
  _sessid     :: SessUUID,
  _seqMap     :: M.Map (ObjType, Key) SeqNo,
  _curTxn     :: Maybe (TxnID, S.Set TxnDep)
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
  sessid <- randomIO
  -- Initialize session
  return $ Session fe sock serverAddr sessid M.empty Nothing

endSession :: Session -> IO ()
endSession s = disconnect (s ^. server) (s^.serverAddr)

beginTxn :: Session -> IO Session
beginTxn s = do
  when (s^.curTxn /= Nothing) $ error "beginTxn: Nested transactions are not supported!"
  txnID <- randomIO
  return $ s {_curTxn = Just (txnID, S.empty)}

endTxn :: Session -> IO Session
endTxn s = do
  when (s^.curTxn == Nothing) $ error "endTxn: No transaction in progress!"
  let (txnID, deps) = fromJust $ s^.curTxn
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
  let req = ReqOper $ OperationPayload objType key operName (encode arg) (s ^. sessid) seqNo
  send (s^.server) [] $ encode req
  responseBlob <- receive (s^.server)
  let (Response newSeqNo resBlob) = decodeResponse responseBlob
  case decode resBlob of
    Left s -> error $ "invoke : decode failure " ++ s
    Right res -> do
      let newSeqMap = M.insert (objType, key) newSeqNo $ s^.seqMap
      let partialSessRV = Session (s^.broker) (s^.server) (s^.serverAddr)
                                  (s^.sessid) newSeqMap
      case s^.curTxn of
        Nothing -> return (res, partialSessRV Nothing)
        Just (txid, deps) ->
          if newSeqNo == seqNo
          then return (res, partialSessRV (Just (txid, deps)))
          else do
            let newDeps = S.insert (TxnDep objType key (s^.sessid) newSeqNo) deps
            return (res, partialSessRV (Just (txid, newDeps)))

newKey :: IO Key
newKey = Key <$> randomIO

mkKey :: UUID -> Key
mkKey uuid = Key uuid

getUUID :: Key -> UUID
getUUID (Key k) = k
