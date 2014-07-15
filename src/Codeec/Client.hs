{-# LANGUAGE TemplateHaskell #-}

module Codeec.Client (
  Session,

  beginSession,
  endSession,
  invoke
) where

import Codeec.Types
import Codeec.NameService.SimpleBroker
import Control.Lens
import System.ZMQ4
import Data.Serialize
import Codeec.Marshall

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

invoke :: (Operation on, Serialize arg, Serialize res)
       => Session -> on -> arg -> IO res
invoke s operName arg = do
  let objType = getObjType operName
  let req = Request objType operName (encode arg)
  send (s^.server) [] $ encode req
  result <- receive (s^.server)
  case decode result of
    Left s -> error $ "invoke : decode failure " ++ s
    Right v -> return v
