{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module RubisDefs (
  ItemID(..),
  WalletID(..),
  BidID(..),
  ItemEffect(..),
  WalletEffect(..),
  BidEffect(..),
  ItemBidEffect(..),
  WalletBidEffect(..),
  WalletItemEffect(..),
  Operation(..),
  createTables,
  dropTables,

  stockItem, stockItemCtrt,
  removeFromStock, removeFromStockCtrt,
  updateMaxBid, updateMaxBidCtrt,
  showItem, showItemCtrt,

  getBalance, getBalanceCtrt,
  depositToWallet, depositToWalletCtrt,
  withdrawFromWallet, withdrawFromWalletCtrt,

  addBid, addBidCtrt,
  cancelBid, cancelBidCtrt,

  addItemBid, addItemBidCtrt,
  removeItemBid, removeItemBidCtrt,
  getBidsByItem, getBidsByItemCtrt,

  addWalletBid, addWalletBidCtrt,
  removeWalletBid, removeWalletBidCtrt,
  getBidsByWallet, getBidsByWalletCtrt,

  addWalletItem, addWalletItemCtrt,
  getItemsByWallet, getItemsByWalletCtrt
) where


import Database.Cassandra.CQL
import Data.Serialize as S
import qualified Data.Map as M
import Data.Time.Clock
import Control.Applicative ((<$>))
import Data.Tuple.Select (sel1)
import Data.DeriveTH
import Data.Maybe (fromJust)

import Codeec.Types
import Codeec.Contract
import Codeec.TH
import Codeec.DBDriver
import Data.List (groupBy, find)


--------------------------------------------------------------------------------
-- ItemTag table : key =

-- data ItemTagEffect = SearchItemsByTag_
--                   | addItemToTag_ ItemID

--------------------------------------------------------------------------------
-- Item table : key = ItemID

newtype ItemID = ItemID Int deriving (Eq, Ord, Show)

data ItemEffect = ShowItem_
                | StockItem_ String{- Desc-} Int{- MinPrice -} Int{- MaxBid -}
                | RemoveFromStock_
                | UpdateMaxBid_ Int{- Bid amount -} deriving Eq

$(derive makeSerialize ''ItemID)

$(derive makeSerialize ''ItemEffect)

instance CasType ItemEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

stockItem :: [ItemEffect] -> (String,Int) -> ((),Maybe ItemEffect)
stockItem _ (desc,mp) = ((),Just $ StockItem_ desc mp 0)

removeFromStock :: [ItemEffect] -> () -> ((), Maybe ItemEffect)
removeFromStock _ _ = ((),Just $ RemoveFromStock_)

updateMaxBid :: [ItemEffect] -> Int -> ((), Maybe ItemEffect)
updateMaxBid _ newbid = ((), Just $ UpdateMaxBid_ newbid)

showItem :: [ItemEffect] -> () -> (Maybe (String,Int,Int), Maybe ItemEffect)
showItem ctxt _ =
  case foldl acc Nothing ctxt of
    Nothing -> (Nothing, Just ShowItem_)
    Just (descOp, minPrice, maxb) ->
      let desc = case descOp of
                   Nothing -> "- No description or minimum price available for this item -"
                   Just d -> d
      in (Just (desc, minPrice, maxb), Just ShowItem_)
  where
    acc :: Maybe (Maybe String, Int, Int) -> ItemEffect -> Maybe (Maybe String, Int, Int)
    acc resOp (StockItem_ desc mp _) =
      let maxb = case resOp of
                    Just (_,_, maxb) -> maxb
                    Nothing -> 0
      in Just (Just desc, mp, maxb)
    acc _ (RemoveFromStock_) = Nothing
    acc resOp (UpdateMaxBid_ newb) =
      case resOp of
        Just (descOp, mp, maxb) -> Just (descOp, mp, max maxb newb )
        Nothing -> Just (Nothing, 0, newb)
    acc resOp ShowItem_ = resOp

instance Effectish ItemEffect where
  summarize ctxt =
    case showItem ctxt () of
      (Nothing, _) -> []
      (Just (d, mp, maxb), _) -> [StockItem_ d mp maxb]

--------------------------------------------------------------------------------
-- Wallets table : Key = WalletId

newtype WalletID = WalletID Int deriving (Eq, Ord)

data WalletEffect = GetBalance_
                  | DepositToWallet_ Int
                  | WithdrawFromWallet_ Int deriving Eq

$(derive makeSerialize ''WalletID)

$(derive makeSerialize ''WalletEffect)

depositToWallet :: [WalletEffect] -> Int -> ((),Maybe WalletEffect)
depositToWallet _ amt = ((),Just $ DepositToWallet_ amt)

withdrawFromWallet :: [WalletEffect] -> Int -> (Bool,Maybe WalletEffect)
withdrawFromWallet ctxt amt =
  let (bal, _) = getBalance ctxt ()
  in if bal > amt
     then (True, Just $ WithdrawFromWallet_ amt)
     else (False, Nothing)

getBalance :: [WalletEffect] -> () -> (Int, Maybe WalletEffect)
getBalance ops () =
  let v = {- trace ("GB : #effect=" ++ (show $ length ops)) -} foldl acc 0 ops
  in (v, Nothing)
  where
    acc s (DepositToWallet_ i) = s + i
    acc s (WithdrawFromWallet_ i) = s - i
    acc s GetBalance_ = s


instance Effectish WalletEffect where
  summarize ctxt =
    let (v, _) = getBalance ctxt ()
    in [DepositToWallet_ v]

instance CasType WalletEffect where
  putCas = put
  getCas = get
  casType _ = CBlob


--------------------------------------------------------------------------------
-- Bids table : Key = BidId

newtype BidID = BidID Int deriving (Eq, Ord)

data BidEffect = AddBid_ {- by -}WalletID {- on -}ItemID{- for amt-}Int
               | GetBid_
               | CancelBid_ deriving Eq

$(derive makeSerialize ''BidID)

$(derive makeSerialize ''BidEffect)

addBid :: [BidEffect] -> (WalletID,ItemID,Int) -> ((), Maybe BidEffect)
addBid _ (wID,itemID,amt) = ((), Just $ AddBid_ wID itemID amt)

cancelBid :: [BidEffect] -> () -> ((), Maybe BidEffect)
cancelBid _ _ = ((), Just CancelBid_)

getBid :: [BidEffect] -> () -> (Maybe (WalletID, ItemID, Int), Maybe BidEffect)
getBid ctxt _ =
  case (find f ctxt, elem CancelBid_ ctxt) of
    (Nothing, False) -> (Nothing, Just GetBid_)
    (Just (AddBid_ x y z), False) -> (Just (x,y,z), Just GetBid_)
    (_,True) -> (Nothing, Just GetBid_)
  where
    f (AddBid_ x y z) = True
    f _ = False

instance Effectish BidEffect where
  summarize l = case getBid l () of
                  (Just (x,y,z), _) -> [AddBid_ x y z]
                  (Nothing,_) -> []

instance CasType BidEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

--------------------------------------------------------------------------------
-- ItemBids table : Key = ItemID

data ItemBidEffect = AddItemBid_ BidID
                   | GetBidsByItem_
                   | RemoveItemBid_ BidID deriving Eq

$(derive makeSerialize ''ItemBidEffect)

addItemBid :: [ItemBidEffect] -> BidID -> ((),Maybe ItemBidEffect)
addItemBid _ bidID = ((),Just $ AddItemBid_ bidID)

removeItemBid :: [ItemBidEffect] -> BidID -> ((), Maybe ItemBidEffect)
removeItemBid ctxt (bidID) = ((), Just $ RemoveItemBid_ bidID)

getBidsByItem :: [ItemBidEffect] -> () -> ([BidID], Maybe ItemBidEffect)
getBidsByItem ctxt _ =
  let (log :: [(BidID,Int)]) = foldr acc [] ctxt in
  let (bidGroups :: [[(BidID,Int)]]) = (\(id,_) (id',_) ->  id == id') `groupBy` log in
  let bidCounts = map grpSummarize bidGroups in
  let validBids = map fst $ filter (\(_,cnt) -> cnt>0) bidCounts in
    (validBids, Just GetBidsByItem_)
  where
    acc :: ItemBidEffect -> [(BidID,Int)] -> [(BidID,Int)]
    acc (AddItemBid_ id) s = (id,1):s
    acc (RemoveItemBid_ id) s = (id,-1):s
    acc GetBidsByItem_ s = s
    grpSummarize :: [(BidID,Int)] -> (BidID,Int)
    grpSummarize l = (fst $ hd l, foldl (\x y -> x+y) 0 (map snd l))
    hd :: [(BidID,Int)] -> (BidID,Int)
    hd (x:xs) = x
    hd _ = error "hd error"

instance Effectish ItemBidEffect where
  summarize ctxt =
    let (bids,_)  = getBidsByItem ctxt () in
      map (\bidID -> AddItemBid_ bidID) bids

instance CasType ItemBidEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

--------------------------------------------------------------------------------
-- WalletBids table : Key = WalletID

data WalletBidEffect = AddWalletBid_ BidID
                    | GetBidsByWallet_
                    | RemoveWalletBid_ BidID deriving Eq

$(derive makeSerialize ''WalletBidEffect)

addWalletBid :: [WalletBidEffect] -> BidID -> ((),Maybe WalletBidEffect)
addWalletBid _ bidID = ((),Just $ AddWalletBid_ bidID)

removeWalletBid :: [WalletBidEffect] -> BidID -> ((), Maybe WalletBidEffect)
removeWalletBid ctxt (bidID) = ((), Just $ RemoveWalletBid_ bidID)

getBidsByWallet :: [WalletBidEffect] -> () -> ([BidID], Maybe WalletBidEffect)
getBidsByWallet ctxt _ =
  let (log :: [(BidID,Int)]) = foldr acc [] ctxt in
  let (bidGroups :: [[(BidID,Int)]]) = (\(id,_) (id',_) ->  id == id') `groupBy` log in
  let bidCounts = map grpSummarize bidGroups in
  let validBids = map fst $ filter (\(_,cnt) -> cnt>0) bidCounts in
    (validBids, Just GetBidsByWallet_)
  where
    acc :: WalletBidEffect -> [(BidID,Int)] -> [(BidID,Int)]
    acc (AddWalletBid_ id) s = (id,1):s
    acc (RemoveWalletBid_ id) s = (id,-1):s
    acc GetBidsByWallet_ s = s
    grpSummarize :: [(BidID,Int)] -> (BidID,Int)
    grpSummarize l = (fst $ hd l, foldl (\x y -> x+y) 0 (map snd l))
    hd :: [(BidID,Int)] -> (BidID,Int)
    hd (x:xs) = x
    hd _ = error "hd error"

instance Effectish WalletBidEffect where
  summarize ctxt =
    let (bids,_)  = getBidsByWallet ctxt () in
      map (\bidID -> AddWalletBid_ bidID) bids

instance CasType WalletBidEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

--------------------------------------------------------------------------------
-- WalletItems table : Key = WalletID

data WalletItemEffect = AddWalletItem_ ItemID
                      | GetItemsByWallet_ deriving Eq

$(derive makeSerialize ''WalletItemEffect)

addWalletItem :: [WalletItemEffect] -> ItemID -> ((),Maybe WalletItemEffect)
addWalletItem _ itemID = ((),Just $ AddWalletItem_ itemID)

getItemsByWallet :: [WalletItemEffect] -> () -> ([ItemID], Maybe WalletItemEffect)
getItemsByWallet ctxt _ = (foldr acc [] ctxt, Just GetItemsByWallet_)
  where
    acc :: WalletItemEffect -> [ItemID] -> [ItemID]
    acc (AddWalletItem_ id) s = id:s
    acc GetItemsByWallet_ s = s

instance Effectish WalletItemEffect where
  summarize ctxt =
    let (items,_)  = getItemsByWallet ctxt () in
      map (\itemID -> AddWalletItem_ itemID) items

instance CasType WalletItemEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

mkOperations [''ItemEffect, ''WalletEffect, ''BidEffect, ''WalletBidEffect,
  ''ItemBidEffect, ''WalletItemEffect]
$(derive makeSerialize ''Operation)

--------------------------------------------------------------------------------
-- Contracts

trueCtrt :: Contract Operation
trueCtrt x = liftProp $ true

stockItemCtrt :: Contract Operation
stockItemCtrt = trueCtrt

removeFromStockCtrt:: Contract Operation
removeFromStockCtrt = trueCtrt

updateMaxBidCtrt :: Contract Operation
updateMaxBidCtrt = trueCtrt

showItemCtrt :: Contract Operation
showItemCtrt = trueCtrt

getBalanceCtrt :: Contract Operation
getBalanceCtrt x = forall_ $ \a -> liftProp $ hbo a x ⇒ vis a x

depositToWalletCtrt :: Contract Operation
depositToWalletCtrt = (trueCtrt :: Contract Operation)

-- As usual, withdraw has to be total-order
withdrawFromWalletCtrt :: Contract Operation
withdrawFromWalletCtrt x = forallQ_ [WithdrawFromWallet] $ \a ->
                              liftProp $ vis a x ∨ vis x a ∨ sameEff a x

addBidCtrt :: Contract Operation
addBidCtrt = trueCtrt

cancelBidCtrt :: Contract Operation
cancelBidCtrt = trueCtrt

addItemBidCtrt :: Contract Operation
addItemBidCtrt = trueCtrt

removeItemBidCtrt :: Contract Operation
removeItemBidCtrt = trueCtrt

getBidsByItemCtrt :: Contract Operation
getBidsByItemCtrt = trueCtrt

addWalletBidCtrt :: Contract Operation
addWalletBidCtrt = trueCtrt

removeWalletBidCtrt :: Contract Operation
removeWalletBidCtrt = trueCtrt

getBidsByWalletCtrt :: Contract Operation
{- It is good to show user all bids he has placed atleast in the
 - current session -}
{- Session Consistency -}
getBidsByWalletCtrt x = forallQ_ [AddWalletBid, RemoveWalletBid] $
                          \a -> liftProp $ soo a x ⇒ vis a x
addWalletItemCtrt :: Contract Operation
addWalletItemCtrt = trueCtrt

getItemsByWalletCtrt :: Contract Operation
{- Seller wants to see all items he is selling -}
{- Session Consistency -}
getItemsByWalletCtrt x = forallQ_ [AddWalletItem] $
                          \a -> liftProp $ soo a x ⇒ vis a x

--------------------------------------------------------------------------------

createTables :: Cas ()
createTables = do
  createTxnTable
  createTable "ItemEffect"
  createTable "WalletEffect"
  createTable "BidEffect"
  createTable "ItemBidEffect"
  createTable "WalletBidEffect"
  createTable "WalletItemEffect"

dropTables :: Cas ()
dropTables = do
  dropTxnTable
  dropTable "ItemEffect"
  dropTable "WalletEffect"
  dropTable "BidEffect"
  dropTable "ItemBidEffect"
  dropTable "WalletBidEffect"
  dropTable "WalletItemEffect"
