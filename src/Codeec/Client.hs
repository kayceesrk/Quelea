{-# LANGUAGE TemplateHaskell #-}

module Codeec.Client (
  Key,
  Session,

  beginSession,
  endSession,
  invoke,
  newKey
) where

import Codeec.Types
import Codeec.NameService.SimpleBroker
import Control.Lens
import System.ZMQ4
import Data.Serialize
import Codeec.Marshall
import System.Random (randomIO)
import Control.Applicative

data Session = Session {
  _broker     :: Frontend,
  _server     :: Socket Req,
  _serverAddr :: String
}

makeLenses ''Session

beginSession :: Frontend -> IO Session
beginSession fe = do
  serverAddr <- clientJoin fe
  ctxt <- context
  sock <- socket ctxt Req
  connect sock serverAddr
  return $ Session fe sock serverAddr

endSession :: Session -> IO ()
endSession s = disconnect (s ^. server) (s^.serverAddr)

invoke :: (OperationClass on, Serialize arg, Serialize res)
       => Session -> Key -> on -> arg -> IO res
invoke s key operName arg = do
  let objType = getObjType operName
  let req = Request objType key operName (encode arg)
  send (s^.server) [] $ encode req
  result <- receive (s^.server)
  case decode result of
    Left s -> error $ "invoke : decode failure " ++ s
    Right v -> return v

newKey :: IO Key
newKey = Key <$> randomIO
