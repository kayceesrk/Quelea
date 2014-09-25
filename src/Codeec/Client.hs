{-# LANGUAGE TemplateHaskell #-}

module Codeec.Client (
  Key,
  Session,

  beginSession,
  endSession,
  invoke,
  newKey,
  mkKey,
  getUUID,
  getServerAddr
) where

import Data.UUID
import Codeec.Types
import Codeec.NameService.SimpleBroker
import Control.Lens
import System.ZMQ4
import Data.Serialize
import Codeec.Marshall
import System.Random (randomIO)
import Control.Applicative
import Data.ByteString (cons)

data Session = Session {
  _broker     :: Frontend,
  _server     :: Socket Req,
  _serverAddr :: String,
  _sessid     :: SessUUID,
  _seqno      :: SeqNo
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
  return $ Session fe sock serverAddr sessid 0

endSession :: Session -> IO ()
endSession s = do
  send (s^.server) [] $ Data.ByteString.cons 1 $ encode (s^.sessid)
  receive $ s^.server
  disconnect (s ^. server) (s^.serverAddr)

getServerAddr :: Session -> String
getServerAddr s = s^.serverAddr

invoke :: (OperationClass on, Serialize arg, Serialize res)
       => Session -> Key -> on -> arg -> IO (res, Session)
invoke s key operName arg = do
  let objType = getObjType operName
  let req = OperationPayload objType key operName (encode arg) (s ^. sessid) (s ^. seqno)
  send (s^.server) [] $ Data.ByteString.cons 0 $ encode req
  responseBlob <- receive (s^.server)
  let (Response newSeqNo resBlob) = decodeResponse responseBlob
  case decode resBlob of
    Left s -> error $ "invoke : decode failure " ++ s
    Right res -> do
      return (res, Session (s^.broker) (s^.server) (s^.serverAddr) (s^.sessid) newSeqNo)

newKey :: IO Key
newKey = Key <$> randomIO

mkKey :: UUID -> Key
mkKey uuid = Key uuid

getUUID :: Key -> UUID
getUUID (Key k) = k
