{-# LANGUAGE TemplateHaskell #-}

module Quelea.ClientMonad (
  Key,
  Session,
  Availability(..),
  TxnKind(..),
  TxnDep,
  CSN,

  runSession,
  runSessionWithStats,
  getStats,
  invoke,
  invokeAndGetDeps,
  newKey,
  mkKey,
  atomically,
  atomicallyWith,
  getServerAddr,
  getLastEffect
) where

import Quelea.Types
import Quelea.Client hiding (invoke, getServerAddr, invokeAndGetDeps, getLastEffect, getStats)
import qualified Quelea.Client as CCLow
import Control.Monad.Trans.State
import Control.Monad.Trans (liftIO)
import Control.Lens
import Quelea.NameService.Types
import Data.Serialize hiding (get, put)
import qualified Data.Set as S
import qualified System.ZMQ4 as ZMQ4
import Data.Time

makeLenses ''Session

type CSN a = StateT Session IO a

runSession :: NameService -> CSN a -> IO a
runSession ns comp = do
  session <- beginSession ns
  res <- evalStateT comp session
  endSession session
  return res

runSessionWithStats :: NameService -> CSN a -> IO a
runSessionWithStats ns comp = do
  session <- beginSessionStats ns True
  res <- evalStateT comp session
  endSession session
  return res

getStats :: CSN (Double, NominalDiffTime)
getStats = do
  session <- get
  liftIO $ CCLow.getStats session

invoke :: (OperationClass on, Serialize arg, Serialize res)
       => Key -> on -> arg -> CSN res
invoke key operName arg = do
  session <- get
  (res, newSession) <- liftIO $ CCLow.invoke session key operName arg
  put newSession
  return res

invokeAndGetDeps :: (OperationClass on, Serialize arg, Serialize res)
       => Key -> on -> arg -> CSN (res, S.Set TxnDep)
invokeAndGetDeps key operName arg = do
  session <- get
  (res, deps, newSession) <- liftIO $ CCLow.invokeAndGetDeps session key operName arg
  put newSession
  return (res, deps)

getServerAddr :: CSN String
getServerAddr = do
  s <- use serverAddr
  return s

atomically :: TxnKind -> CSN a -> CSN a
atomically tk m = do
  get >>= liftIO . (flip beginTxn tk) >>= put
  r <- m
  get >>= liftIO . (endTxn S.empty) >>= put
  return r

atomicallyWith :: S.Set TxnDep -> TxnKind -> CSN a -> CSN a
atomicallyWith extraDeps tk m = do
  get >>= liftIO . (flip beginTxn tk) >>= put
  r <- m
  get >>= liftIO . (endTxn extraDeps) >>= put
  return r

getLastEffect :: CSN (Maybe TxnDep)
getLastEffect = do
  s <- get
  return $ s^.lastEffect
