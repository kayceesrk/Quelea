{-# LANGUAGE TemplateHaskell #-}

module Codeec.ClientMonad (
  Key,
  Session,

  runSession,
  invoke,
  newKey,
  atomically,
  getServerAddr,
) where

import Codeec.Types
import Codeec.Client hiding (invoke, getServerAddr)
import qualified Codeec.Client as CCLow
import Control.Monad.Trans.State
import Control.Monad.Trans (liftIO)
import Control.Lens
import Codeec.NameService.SimpleBroker
import Data.Serialize hiding (get, put)

makeLenses ''Session

type CSN a = StateT Session IO a

runSession :: Frontend -> CSN a -> IO a
runSession fe comp = do
  session <- beginSession fe
  res <- evalStateT comp session
  endSession session
  return res

invoke :: (OperationClass on, Serialize arg, Serialize res)
       => Key -> on -> arg -> CSN res
invoke key operName arg = do
  session <- get
  (res, newSession) <- liftIO $ CCLow.invoke session key operName arg
  put newSession
  return res

getServerAddr :: CSN String
getServerAddr = do
  s <- use serverAddr
  return s

atomically :: CSN a -> CSN a
atomically m = do
  get >>= liftIO . beginTxn >>= put
  r <- m
  get >>= liftIO . endTxn >>= put
  return r
