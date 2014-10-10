{-# LANGUAGE TemplateHaskell #-}

module Codeec.ClientMonad (
  Key,
  Session,
  TxnKind(..),
  CSN,

  runSession,
  invoke,
  invokeAndGetDeps,
  newKey,
  mkKey,
  atomically,
  atomicallyWith,
  getServerAddr,
  getLastEffect
) where

import Codeec.Types
import Codeec.Client hiding (invoke, getServerAddr, invokeAndGetDeps, getLastEffect)
import qualified Codeec.Client as CCLow
import Control.Monad.Trans.State
import Control.Monad.Trans (liftIO)
import Control.Lens
import Codeec.NameService.SimpleBroker
import Data.Serialize hiding (get, put)
import qualified Data.Set as S

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
