{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module TPCCTxns (
  doNewOrderTxn,
  doPaymentTxn,
  doDeliveryTxn
) where

import Quelea.ClientMonad
import Quelea.TH (checkTxn)

import TPCCDefs
import TPCCCtrts

import Control.Monad.Trans (liftIO)
import Data.Time.Clock
import System.Random (randomIO)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Data.List
import Data.UUID
import qualified Data.Set as S
import Control.Monad (foldM, when)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO (hFlush, stdout)

[doNewOrderTxnCtrtA,
 doPaymentTxnCtrtA,
 doDeliveryTxnCtrtA] =
   $(do
      t1 <- runIO getCurrentTime
      a1 <- checkTxn "doNewOrderTxn" doNewOrderTxnCtrt
      a2 <- checkTxn "doPaymentTxn" doPaymentTxnCtrt
      a3 <- checkTxn "doDeliveryTxn" doDeliveryTxnCtrt
      le <- return $ (ListE::[Exp] -> Exp) [a1]
      t2 <- runIO getCurrentTime
      _ <- runIO $ putStrLn $ "----------------------------------------------------------"
      _ <- runIO $ putStrLn $ "Classification of transaction contracts completed in "++
                (show $ diffUTCTime t2 t1)++"."
      _ <- runIO $ putStrLn $ "----------------------------------------------------------"
      _ <- runIO $ hFlush stdout
      return le)

doNewOrderTxn :: DistrictID -> WarehouseID -> Int -> CSN (Int)
doNewOrderTxn did wid ireq = 
  let ireqs = [1..ireq] in
  atomically (doNewOrderTxnCtrtA) $ do
    nextoid::Int <- invoke (mkKey (did,wid)) GetAndIncNextOID ()
    r::() <- invoke (mkKey (nextoid,did,wid)) AddOrder (length ireqs)
    r::() <- foldM (\ _ x -> invoke (mkKey (nextoid,did,wid,x)) AddOrderline ()) () ireqs
    return nextoid

doPaymentTxn :: Int -> WarehouseID -> DistrictID -> CustomerID -> CSN ()
doPaymentTxn h_amt wid cdid cid = 
  atomically (doPaymentTxnCtrtA) $ do
    r::() <- invoke (mkKey wid) AddYtd (h_amt)
    r::() <- invoke (mkKey (cid,cdid,wid)) AddCustomerBal (-1*h_amt)
    r::() <- invoke (mkKey (cid,cdid,wid)) AddHistoryAmt (h_amt)
    return ()

doDeliveryTxn :: WarehouseID -> DistrictID -> OrderID -> CSN ()
doDeliveryTxn wid did oid = do
  ol_cnt::Int <- invoke (mkKey (oid,did,wid)) GetOlCnt ()
  let ireqs = [1..ol_cnt]
  atomically (doDeliveryTxnCtrtA) $ do
    r::() <- invoke (mkKey (oid,did,wid)) SetCarrier ()
    r::() <- foldM (\ _ x -> invoke (mkKey (oid,did,wid,x)) SetDeliveryDate ()) () ireqs
    return ()

{-Idea to run txns in the run time:
  // did, wid, cid are randomly initiated uuids
  run did wid cid =
  First, customer buys some stuff
  oid <- doNewOrderTxn did wid ireq //ireq is a random number
  Then, the stuff gets delivered
  () <- doDeliveryTxn wid did oid 
  Then, the customer pays
  () <- doPaymentTxn h_amt wid did cid //h_amt is a random number-}