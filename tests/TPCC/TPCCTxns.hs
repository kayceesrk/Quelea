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
      le <- return $ (ListE::[Exp] -> Exp) [a1, a2, a3]
      t2 <- runIO getCurrentTime
      _ <- runIO $ putStrLn $ "----------------------------------------------------------"
      _ <- runIO $ putStrLn $ "Classification of transaction contracts completed in "++
                (show $ diffUTCTime t2 t1)++"."
      _ <- runIO $ putStrLn $ "----------------------------------------------------------"
      _ <- runIO $ hFlush stdout
      return le)

doNewOrderTxn :: DistrictID -> WarehouseID -> Int -> CSN OrderID
doNewOrderTxn did wid ireq =
  let ireqs = [1..ireq] in
  atomically (doNewOrderTxnCtrtA) $ do
    liftIO $ putStrLn "GetAndIncNextOID"
    nextoid::Int <- invoke (mkKey (did,wid)) GetAndIncNextOID True
    liftIO $ putStrLn "AddOrder"
    r::() <- invoke (mkKey (nextoid,did,wid)) AddOrder (length ireqs)
    r::() <- foldM (\ _ x -> do
               liftIO $ putStrLn "AddOrderLine"
               invoke (mkKey (nextoid,did,wid,x)) AddOrderline ()) () ireqs
    return $ OrderID nextoid

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

{-Invariant corresponds to doPaymentTxn-}
{-W_YTD = sum(H_AMOUNT) where history entry's cwid = this warehouse's id-}
inv1 :: WarehouseID -> CSN (Bool)
inv1 wid = do
  ks::[(CustomerID, DistrictID, WarehouseID)] <- getKeys "HistoryEffect"
  w_ytd::Int <- invoke (mkKey (wid)) GetYtd ()
  h_amt::Int <- foldM (\ s (cid,cdid,cwid) ->
                        if cwid==wid then
                          do
                          r::Int <- invoke (mkKey (cid,cdid,cwid)) GetHistoryAmt ()
                          return $ r+s
                        else return s) 0 ks
  return $ w_ytd==h_amt

{- Invariant corresponds to doNewOrderTxn -}
{- District's NEXT_O_ID - 1 = max(OrderID) where OrderID's district is this
   District'd did and OrderID's warehouse is this district's warehouse(dwid) . -}
inv2 :: DistrictID -> WarehouseID -> CSN (Bool)
inv2 (DistrictID did) (WarehouseID dwid) = do
  ks_d::[(DistrictID, WarehouseID)] <- getKeys "DistrictEffect"
  ks_o::[(OrderID, DistrictID, WarehouseID)] <- getKeys "OrderEffect"
  max_oid::Int <- foldM (\ s (OrderID o_id, DistrictID o_d_id, WarehouseID o_w_id) ->
                        if o_d_id==did && o_w_id==dwid then
                          if o_id > s then return o_id else return s
                        else return s) (-1) ks_o
  d_nextoid::Int <- foldM (\ s (DistrictID d_id, WarehouseID d_w_id) ->
                        if did==d_id && dwid==d_w_id then
                          do
                          nextoid::Int <- invoke (mkKey (d_id,d_w_id)) GetAndIncNextOID (False)
                                                                      {-Don't increment-}
                          return nextoid
                        else return s) 0 ks_d
  return $ max_oid == d_nextoid-1

{- Invariant corresponds to doDeliveryTxn -}
{-For any row in the ORDER-LINE table, DeliveryDate is set
   if and only if the corresponding row in the ORDER table defined by
   (O_W_ID, O_D_ID, O_ID) = (OL_W_ID, OL_D_ID, OL_O_ID)
   has O_CARRIER_ID set-}
inv3 :: WarehouseID -> DistrictID -> OrderID -> CSN (Bool)
inv3 o_w_id o_d_id o_id = do
  ks_ol::[(OrderID, DistrictID, WarehouseID, Int)] <- getKeys "OrderlineEffect"
  o_carrier_set::Bool <- invoke (mkKey (o_id,o_d_id,o_w_id)) CheckCarrierSet ()
  ol_delivery_set::Bool <- foldM (\ s ((ol_o_id,ol_d_id,ol_w_id,olnum)) ->
                if o_d_id==ol_d_id && o_w_id==ol_w_id && o_id==ol_o_id then
                  do
                  r::Bool <- invoke (mkKey (ol_o_id,ol_d_id,ol_w_id,olnum)) CheckDeliverySet ()
                  return $ r || s
                else return s) False ks_ol
  return $ if o_carrier_set then ol_delivery_set else (not ol_delivery_set)

{-Idea to run txns in the run time:
  // did, wid, cid are randomly initiated uuids
  run did wid cid =
  First, customer buys some stuff
  oid <- doNewOrderTxn did wid ireq //ireq is a random number > 0
  Then, the stuff gets delivered
  () <- doDeliveryTxn wid did oid
  Then, the customer pays
  () <- doPaymentTxn h_amt wid did cid //h_amt is a random number-}
