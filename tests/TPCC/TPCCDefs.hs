{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module TPCCDefs (
  WarehouseID(..),
  DistrictID(..),
  OrderID(..),
  CustomerID(..),
  Operation(..),
  createTables,
  dropTables,

  -- Warehouse
  addYtd, addYtdCtrt,
  getYtd, getYtdCtrt,

  -- District
  getandIncNextOID, getandIncNextOIDCtrt,
  --getNextOID, getNextOIDCtrt,

  -- Customer
  addCustomerBal, addCustomerBalCtrt,
  --getCustomer, getCustomerCtrt,

  -- History
  addHistoryAmt, addHistoryAmtCtrt,
  getHistoryAmt, getHistoryAmtCtrt, 

  -- Order
  setCarrier, setCarrierCtrt,
  addOrder, addOrderCtrt,
  getOlCnt, getOlCntCtrt,
  checkCarrierSet, checkCarrierSetCtrt,
  {-getOrder, getOrderCtrt,-}

  -- Orderline
  addOrderline, addOrderlineCtrt,
  setDeliveryDate, setDeliveryDateCtrt,
  checkDeliverySet, checkDeliverySetCtrt
  --getOrderline, getOrderlineCtrt
  ) where

import Database.Cassandra.CQL as CQL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize as S
import Data.Word (Word8)
import Control.Applicative ((<$>))
import Quelea.Types
import Quelea.Contract
import Quelea.TH
import Debug.Trace
import Data.DeriveTH
import Data.UUID
import Quelea.DBDriver
import Data.List (groupBy, find)

{-Warehouse Table : key (WarehouseID) -}

newtype WarehouseID = WarehouseID UUID deriving (Eq, Ord)

data WarehouseEffect = GetYtd_
                    | AddYtd_ Int{- Ytd -} deriving Eq

$(derive makeSerialize ''WarehouseID)

$(derive makeSerialize ''WarehouseEffect)

instance CasType WarehouseEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

addYtd :: [WarehouseEffect] -> (Int) -> ((), Maybe WarehouseEffect)
addYtd _ (ytd) = ((),Just $ AddYtd_ ytd)

getYtd :: [WarehouseEffect] -> () -> ((Int), Maybe WarehouseEffect)
getYtd ops _ =
  let v = foldl acc 0 ops
  in (v, Nothing)
  where
    acc s (AddYtd_ ytd) = s+ytd
    acc s GetYtd_ = s

instance Effectish WarehouseEffect where
  summarize ctxt = ctxt

{- District Table : key (WarehouseID, DistrictID) -}

newtype DistrictID = DistrictID UUID deriving (Eq, Ord)

data DistrictEffect = GetNextOID_
                  | GetAndIncNextOID_ deriving Eq

$(derive makeSerialize ''DistrictID)
$(derive makeSerialize ''DistrictEffect)

instance CasType DistrictEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

getandIncNextOID :: [DistrictEffect] -> (Bool) -> ((Int), Maybe DistrictEffect)
getandIncNextOID ops inc{-If set then increment-} =
  let v = foldl acc 0 ops
  in 
  if inc then (v, Just $ GetAndIncNextOID_) else (v, Nothing)
  where
    acc s GetAndIncNextOID_ = s+1
    acc s GetNextOID_ = s

instance Effectish DistrictEffect where
  summarize ctxt = ctxt

{-Customer Table: key (CustomerID, DistrictID, WarehouseID) -}

newtype CustomerID = CustomerID UUID deriving (Eq, Ord)

data CustomerEffect = GetCustomerBal_
                    | AddCustomerBal_ Int{- Amt -} deriving Eq

$(derive makeSerialize ''CustomerID)
$(derive makeSerialize ''CustomerEffect)

instance CasType CustomerEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

addCustomerBal :: [CustomerEffect] -> (Int) -> ((),Maybe CustomerEffect)
addCustomerBal _ (amt) = ((),Just $ AddCustomerBal_ amt)

instance Effectish CustomerEffect where
  summarize ctxt = ctxt

{-History Table: key (CustomerID, DistrictID, WarehouseID -}

data HistoryEffect = GetHistoryAmt_
                    | AddHistoryAmt_ Int{- Amt -} deriving Eq

$(derive makeSerialize ''HistoryEffect)

instance CasType HistoryEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

addHistoryAmt :: [HistoryEffect] -> (Int) -> ((),Maybe HistoryEffect)
addHistoryAmt _ (amt) = ((),Just $ AddHistoryAmt_ amt)

getHistoryAmt :: [HistoryEffect] -> () -> ((Int), Maybe HistoryEffect)
getHistoryAmt ops _ = 
  let v = foldl acc 0 ops
  in (v, Nothing)
  where
    acc s (AddHistoryAmt_ amt) = s+amt
    acc s _ = s

instance Effectish HistoryEffect where
  summarize ctxt = ctxt

{-Order Table: key (OrderID, DistrictID, WarehouseID) -}

newtype OrderID = OrderID Int deriving (Eq, Ord)

data OrderEffect = GetOlCnt_
                  | AddOrder_ Int {- o_ol_cnt -} 
                  | CheckCarrierSet_
                  | SetCarrier_ deriving Eq

$(derive makeSerialize ''OrderID)

$(derive makeSerialize ''OrderEffect)

instance CasType OrderEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

addOrder :: [OrderEffect] -> (Int{-o_ol_cnt-}) -> ((),Maybe OrderEffect)
addOrder _ o_ol_cnt = ((),Just $ AddOrder_ o_ol_cnt)

getOlCnt :: [OrderEffect] -> () -> ((Int), Maybe OrderEffect)
getOlCnt [] _ = (0, Nothing)
getOlCnt (AddOrder_ o_ol_cnt:_) _ = (o_ol_cnt, Nothing)

setCarrier :: [OrderEffect] -> () -> ((),Maybe OrderEffect)
setCarrier _ _ = ((),Just $ SetCarrier_)

checkCarrierSet :: [OrderEffect] -> () -> ((Bool),Maybe OrderEffect)
checkCarrierSet [] _ = (False, Nothing)
checkCarrierSet (SetCarrier_:_) _ = (True, Nothing)

instance Effectish OrderEffect where
  summarize ctxt = ctxt

{- Orderline Table : key (OrderID, DistrictID, WarehouseID, Olnum:int) -}

newtype OrderlineTableID = OrderlineTableID (OrderID, DistrictID, WarehouseID, Int) deriving (Eq, Ord)

data OrderlineEffect = Get_
                  | AddOrderline_ 
                  | CheckDeliverySet_
                  | SetDeliveryDate_ deriving Eq


$(derive makeSerialize ''OrderlineTableID)

$(derive makeSerialize ''OrderlineEffect)

instance CasType OrderlineEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

addOrderline :: [OrderlineEffect] -> () -> ((),Maybe OrderlineEffect)
addOrderline _ _ = ((),Just $ AddOrderline_)

setDeliveryDate :: [OrderlineEffect] -> () -> ((),Maybe OrderlineEffect)
setDeliveryDate _ _ = ((),Just $ SetDeliveryDate_)

checkDeliverySet :: [OrderlineEffect] -> () -> ((Bool),Maybe OrderlineEffect)
checkDeliverySet [] _ = (False, Nothing)
checkDeliverySet (SetDeliveryDate_:_) _ = (True, Nothing)

getolRowCnt :: [OrderlineEffect] -> (WarehouseID) -> ((Int),Maybe OrderlineEffect)
getolRowCnt ops (o_w_id) =
  let v = foldl acc 0 ops
  in (v, Nothing)
  where
    acc s AddOrderline_ = s+1
    acc s Get_ = s

instance Effectish OrderlineEffect where
  summarize ctxt = ctxt

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

mkOperations [''WarehouseEffect, ''HistoryEffect, ''DistrictEffect, ''OrderEffect,
              ''OrderlineEffect, ''CustomerEffect]
$(derive makeSerialize ''Operation)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Contracts

trueCtrt :: Contract Operation
trueCtrt x = liftProp $ true

addYtdCtrt :: Contract Operation
addYtdCtrt = trueCtrt

getYtdCtrt :: Contract Operation
getYtdCtrt = trueCtrt

addCustomerBalCtrt :: Contract Operation
addCustomerBalCtrt = trueCtrt

addHistoryAmtCtrt :: Contract Operation
addHistoryAmtCtrt = trueCtrt

getHistoryAmtCtrt :: Contract Operation
getHistoryAmtCtrt = trueCtrt

checkCarrierSetCtrt :: Contract Operation
checkCarrierSetCtrt = trueCtrt

getandIncNextOIDCtrt :: Contract Operation
getandIncNextOIDCtrt x = forallQ_ [GetAndIncNextOID] $ \a -> liftProp $
                        (vis a x ∨ vis x a ∨ appRel SameEff x a)

setDeliveryDateCtrt :: Contract Operation
setDeliveryDateCtrt = trueCtrt

checkDeliverySetCtrt :: Contract Operation
checkDeliverySetCtrt = trueCtrt

setCarrierCtrt :: Contract Operation
setCarrierCtrt = trueCtrt

addOrderCtrt :: Contract Operation
addOrderCtrt = trueCtrt

addOrderlineCtrt :: Contract Operation
addOrderlineCtrt = trueCtrt

getOlCntCtrt :: Contract Operation
getOlCntCtrt = trueCtrt
--------------------------------------------------------------------------------

createTables :: Cas ()
createTables = do
  createTxnTable
  createTable "WarehouseEffect"
  createTable "HistoryEffect"
  createTable "DistrictEffect"
  createTable "OrderEffect"
  createTable "OrderlineEffect"
  createTable "CustomerEffect"

dropTables :: Cas ()
dropTables = do
  dropTxnTable
  dropTable "WarehouseEffect"
  dropTable "HistoryEffect"
  dropTable "DistrictEffect"
  dropTable "OrderEffect"
  dropTable "OrderlineEffect"
  dropTable "CustomerEffect"