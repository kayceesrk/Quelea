{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module RubisTxns (
  newWallet,
  getWalletBalance,

  newItem,
  ItemStatus(..),
  getItem,

  openAuction,
  concludeAuction,
  showMyAuctions,

  BidResult(..),
  bidForItem,
  amIMaxBidder,
  cancelMyBid,
  showMyBids,
) where

import Codeec.ClientMonad
import Codeec.TH (checkTxn)

import RubisDefs
import RubisCtrts

import Control.Monad.Trans (liftIO)
import System.Random (randomIO)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Control.Monad (foldM, when)
import Control.Exception.Base
import Data.Time
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO (hFlush, stdout)


[bidForItemTxnCtrtA,
 showMyBidsTxnCtrtA,
 cancelBidTxnCtrtA,
 openAuctionTxnCtrtA,
 showMyAuctionsTxnCtrtA,
 concludeAuctionTxnCtrtA] =
   $(do
      t1 <- runIO getCurrentTime
      a1 <- checkTxn "bidForItemTxn" bidForItemTxnCtrt
      a2 <- checkTxn "showMyBidsTxn" showMyBidsTxnCtrt
      a3 <- checkTxn "cancelBidTxn" cancelBidTxnCtrt
      a4 <- checkTxn "openAuctionTxn" openAuctionTxnCtrt
      a5 <- checkTxn "showMyAuctionsTxn" showMyAuctionsTxnCtrt
      a6 <- checkTxn "concludeAuctionTxn" concludeAuctionTxnCtrt
      le <- return $ (ListE::[Exp] -> Exp) [a1,a2,a3,a4,a5,a6]
      t2 <- runIO getCurrentTime
      _ <- runIO $ putStrLn $ "----------------------------------------------------------"
      _ <- runIO $ putStrLn $ "Classification of transaction contracts completed in "++
                (show $ diffUTCTime t2 t1)++"."
      _ <- runIO $ putStrLn $ "----------------------------------------------------------"
      _ <- runIO $ hFlush stdout
      return le)

newWallet :: Int {- inital deposit -} -> CSN WalletID
newWallet amt = do
  wid <- liftIO $ randomIO
  r::() <- invoke (mkKey $ WalletID $ wid) DepositToWallet amt
  return $ WalletID wid

getWalletBalance :: WalletID -> CSN Int
getWalletBalance wid = invoke (mkKey $ wid) GetBalance ()

newItem :: String {- description -} -> Int {- min price -} -> CSN ItemID
newItem desc minPrice = do
  iid <- liftIO $ randomIO
  r::() <- invoke (mkKey $ ItemID $ iid) StockItem (desc, minPrice)
  return $ ItemID iid

data ItemStatus = ItemRemoved | ItemAvailable (String, Int, Int) | WaitingForItem

getItem :: ItemID -> CSN ItemStatus
getItem iid = do
  res <- invoke (mkKey $ iid) ShowItem ()
  case res of
    Nothing -> return WaitingForItem
    Just (a,b,c,True) -> return ItemRemoved
    Just (a,b,c,False) -> return $ ItemAvailable (a,b,c)


data BidResult = ItemGone | OutBid | BidSuccess BidID | ItemNotYetAvailable

-- User places a bid for ItemID. The bid is placed only if the item is
-- still available and the bid amount is greater than min price
-- decided by the seller.
-- User won't be billed until he wins the auction.
bidForItem :: WalletID -> ItemID -> Int -> CSN BidResult
bidForItem wid id amt =
  atomically (bidForItemTxnCtrtA) $ do
    resOp  <- getItem id
    case resOp of
      WaitingForItem -> return $ ItemNotYetAvailable
      ItemRemoved -> return $ ItemGone
      ItemAvailable (desc, mp, maxb) -> do
        when (amt < mp) $ error "Bid amount less than min price"
        if amt > maxb
        then do
          bidID <- liftIO $ BidID <$> randomIO
          r::() <- invoke (mkKey bidID) AddBid (wid,id,amt)
          r::() <- invoke (mkKey id) AddItemBid bidID
          r::() <- invoke (mkKey wid) AddWalletBid bidID
          r::() <- invoke (mkKey id) UpdateMaxBid amt
          return $ BidSuccess bidID
        else return $ OutBid

bidIdFold :: [(ItemID,Int)] -> BidID -> CSN [(ItemID,Int)]
bidIdFold acc (bidID) = do
  resOp :: Maybe (WalletID,ItemID,Int)  <- invoke (mkKey bidID) GetBid ()
  let (_,id,amt) =
        case resOp of
          Nothing -> error "bidIdFold"
          Just x -> x
  return $ (id,amt):acc

-- Get all my bids.
showMyBids :: WalletID -> CSN [(ItemID,Int{-amt-})]
showMyBids wid =
  atomically (showMyBidsTxnCtrtA) $ do
    bidIDs::[BidID] <- invoke (mkKey wid) GetBidsByWallet ()
    bids <- foldM bidIdFold [] bidIDs
    return bids

-- This transaction removes things from bids table, and two other
-- materialized views. For consistency, it needs MAV.
-- We do a trivial withdraw of $0 immediately after we cancel the bid.
-- The total-order contract of withdraw, coupled with atomicity on
-- this transaction, means that a different MAV transaction that sees
-- withdraw from this transaction will also see CancelBid. This
-- property will be used in concludeAuction to rule out canceled bids
-- while determining the winner of the auction.
cancelMyBid :: BidID -> CSN ()
cancelMyBid bidID =
  atomically (cancelBidTxnCtrtA) $ do
    resOp :: Maybe (WalletID,ItemID,Int) <- invoke (mkKey bidID) GetBid ()
    let (wID,itemID,_) = case resOp of
              Just x -> x
              Nothing -> error "Cannot Cancel Bid. Please try again."
    r::()<- invoke (mkKey wID) WithdrawFromWallet (0::Int)
    r::() <- invoke (mkKey bidID) CancelBid ()
    r::() <- invoke (mkKey itemID) RemoveItemBid (itemID)
    r::() <- invoke (mkKey wID) RemoveWalletBid (wID)
    return ()

-- Put up an item for auction.
openAuction :: WalletID -> ItemID -> String{-desc-} -> Int{-minPrice-} -> CSN ()
openAuction wid id desc mp =
  atomically (openAuctionTxnCtrtA) $ do
    r::() <- invoke (mkKey id) StockItem (desc,mp,0::Int)
    r::() <- invoke (mkKey wid) AddWalletItem (id)
    return ()

-- Show all items that I offered for auction, along with a local
-- maxbid.
showMyAuctions :: WalletID -> CSN [(ItemID,String,Int{-minPrice-},Int{-maxBid-})]
showMyAuctions wid =
  atomically (showMyAuctionsTxnCtrtA) $ do
    itemIDs <- invoke (mkKey wid) GetItemsByWallet ()
    items <- foldM f [] itemIDs
    return items
      where
        f :: [(ItemID,String,Int,Int)] -> ItemID -> CSN [(ItemID,String,Int,Int)]
        f acc (itemID) = do
          resOp <- invoke (mkKey itemID) ShowItem ()
          let (desc,mp,maxb,status::Bool) = case fromJust resOp of {Nothing -> error "show my auctions"; Just x -> x}
          return $ (itemID,desc,mp,maxb):acc

-- For each bidID in bidIDs, query details of the bidID, and determine
-- maxbidder.
-- Note: Two invocations of getMaxBidFromList with same list of bidIDs may
-- return different maxbidder. This happens if maxbidder from previous
-- invocation has cancelled his bid meanwhile.
getMaxBidFromList :: [BidID] -> CSN (Maybe (BidID,WalletID, Int{-maxbid-}))
getMaxBidFromList bidIDs = foldM f Nothing bidIDs
  where
    f accOp bidID = do
      (resOp::Maybe (WalletID,ItemID,Int))<- invoke (mkKey bidID) GetBid ()
      case (resOp,accOp) of
        (Nothing,_) -> return accOp
        (Just (wid,_,amt), Nothing) -> return $ Just (bidID,wid,amt)
        (Just (wid,_,amt::Int), Just (_,_,amt')) ->
          return $ if amt>amt' then Just (bidID,wid,amt) else accOp

-- Bills best bidder, and returns billed amount. The function
-- terminates because the list of bidIDs in recursive calls
-- monotonically decreases.
billBestBidder :: [BidID] -> CSN Int
billBestBidder bidIDs = do
  resOp <- getMaxBidFromList bidIDs
  case resOp of
    Nothing -> return 0
    Just (bidID,wid,maxbid) ->
      let f b = b/=bidID in
      let billNextBest :: () -> CSN Int
          billNextBest () = billBestBidder $ f `filter` bidIDs in
        do
          r::Bool <- invoke (mkKey wid) WithdrawFromWallet (maxbid::Int)
          if r
          then do -- Best bidder is succesfully billed
            (resOp :: Maybe (WalletID,Int,Int))<- invoke (mkKey bidID) GetBid ()
            case resOp of
              Just _ -> return maxbid -- Best bid is not cancelled
              Nothing -> do -- If best bid is cancelled, deposit money back
                r::() <- invoke (mkKey wid) DepositToWallet (maxbid)
                billNextBest () -- Find next best
          else do
            billNextBest ()

-- This transaction needs to be MAV to ensure that this is in total
-- order with all cancelBid transactions.
concludeAuction :: WalletID{-seller-} -> ItemID -> CSN Int {-max bid -}
concludeAuction wID itemID = do
  let ikey = mkKey itemID
  -- Remove the item from stock to prevent future bids from
  -- being placed on this item.
  r::() <- invoke ikey RemoveFromStock ()
  atomically (concludeAuctionTxnCtrtA) $ do
    let (wkey,ikey) =  (mkKey wID, mkKey itemID)
    -- Get all bids on the item
    bidIDs <- invoke ikey GetBidsByItem ()
    -- Among bidIDs, find best bid and bill best bidder.
    amt::Int <- billBestBidder bidIDs
    r::() <- if amt>0 then invoke wkey DepositToWallet (amt) else return ()
    return amt


amIMaxBidder :: ItemID -> WalletID -> CSN Bool
amIMaxBidder iid wid = do
  bidIDs <- invoke (mkKey iid) GetBidsByItem ()
  maxInfo <- getMaxBidFromList bidIDs
  case maxInfo of
    Nothing -> return False
    Just (_,maxWid,_) -> if wid == maxWid then return True else return False

