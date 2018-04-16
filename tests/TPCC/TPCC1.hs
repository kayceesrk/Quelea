{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, CPP #-}

import Quelea.Shim
import Quelea.ClientMonad
import Quelea.DBDriver
import Quelea.Contract
import Control.Concurrent (ThreadId, myThreadId, forkIO, threadDelay, killThread)
import Quelea.NameService.Types
import Quelea.Types (summarize)
import Quelea.Marshall
import Quelea.TH
#ifdef LBB
import Quelea.NameService.LoadBalancingBroker
#else
import Quelea.NameService.SimpleBroker
#endif

import Language.Haskell.TH 
import Language.Haskell.TH.Syntax (Exp (..))
-- import System.Process (runCommand, terminateProcess)
import System.Posix.Process (forkProcess)
import System.Posix.Signals (signalProcess, killProcess, internalAbort)
import System.Environment (getExecutablePath, getArgs)
import Database.Cassandra.CQL
import Control.Monad.Trans (liftIO)
import Data.Text (pack)
import Quelea.Types (summarize)
import Control.Monad (replicateM_, void, ap)
import Control.Concurrent (threadDelay)
import Data.Time
import System.IO (hFlush, stdout)
import Data.Serialize

import TPCCDefs
import TPCCCtrts
import TPCCTxns

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


data Kind = B | C | S | D | Drop deriving (Read, Show)

[
  addYtdCtrtA,
  getandIncNextOIDCtrtA,
  addHistoryAmtCtrtA, 
  addOrderCtrtA,
  addOrderlineCtrtA,
  getOrderlineCtrtA,
  addCustomerBalCtrtA,
  setCarrierCtrtA,
  setDeliveryDateCtrtA,
  getOlCntCtrtA] = 
    $(do 
        t1 <- runIO getCurrentTime
        a <- (checkOp AddYtd addYtdCtrt)
        c <- (checkOp GetAndIncNextOID getandIncNextOIDCtrt)
        d <- (checkOp AddHistoryAmt addHistoryAmtCtrt)
        f <- (checkOp AddOrder addOrderCtrt)
        h <- (checkOp AddOrderline addOrderlineCtrt)
        i <- (checkOp AddCustomerBal addCustomerBalCtrt)
        j <- (checkOp SetCarrier setCarrierCtrt)
        k <- (checkOp SetDeliveryDate setDeliveryDateCtrt)
        l <- (checkOp GetOlCnt getOlCntCtrt)
        le <- return $ (ListE::[Exp] -> Exp) 
                [a, c, d, f, h, i, j, k, l]
        t2 <- runIO getCurrentTime
        _ <- runIO $ putStrLn $ "----------------------------------------------------------"
        _ <- runIO $ putStrLn $ "Classification of operation contracts completed in "++
                  (show $ diffUTCTime t2 t1)++"."
        _ <- runIO $ putStrLn $ "----------------------------------------------------------"
        _ <- runIO $ hFlush stdout
        return le)

dtLib = mkDtLib [(AddYtd, mkGenOp addYtd summarize, addYtdCtrtA),
                 (GetAndIncNextOID, mkGenOp getandIncNextOID summarize, getandIncNextOIDCtrtA),
                 (AddHistoryAmt, mkGenOp addHistoryAmt summarize, addHistoryAmtCtrtA)
                 (AddOrder, mkGenOp addOrder summarize, addOrderCtrtA),
                 (AddOrderline, mkGenOp addOrderline summarize, addOrderlineCtrtA)
                 (AddCustomerBal, mkGenOp addCustomerBal summarize, addCustomerBalCtrtA)
                 (SetCarrier, mkGenOp setCarrier summarize, setCarrierCtrtA),
                 (SetDeliveryDate, mkGenOp setDeliveryDate summarize, setDeliveryDateCtrtA),
                 (GetOlCnt, mkGenOp getOlCnt summarize, getOlCntCtrtA)]