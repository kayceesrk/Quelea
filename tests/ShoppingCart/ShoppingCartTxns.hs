{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module ShoppingCartTxns (
  openSite,
  searchItem,
  addToCart,
  removeFromCart,
  checkOut
) where

import Codeec.ClientMonad
import Codeec.TH (checkTxn)

import ShoppingCartDefs
import ShoppingCartCtrts

import Control.Monad.Trans (liftIO)
import System.Random (randomIO)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Control.Monad (foldM, when)

openSite :: () -> CSN CartID
openSite () = do
  cid <-liftIO $ CartID <$> randomIO
  r::() <- invoke (mkKey cid) AddCart ()
  return cid

searchItem :: ()
searchItem = ()

billItems :: [(ItemID,{-Pos-}Int)] -> CSN [(ItemID,{-Pos-}Int,Int)]
billItems iqlist = foldM f [] iqlist
  where
    f acc (id,qty) = do
      (desc::String,p::Int,avlblQty::Int) <- invoke (mkKey id) ShowItem ()
      let sqty = if avlblQty >= qty 
                 then qty
                 else avlblQty
      return $ if sqty>0 then (id,sqty,sqty*p):acc else acc

summarizeCart :: CartID -> CSN [(ItemID,Int,Int)]
summarizeCart cartID = do
  let key = mkKey cartID
  iqlist::[(ItemID,Int)] <- invoke key GetCartSummary ()
  let iqlist2::[(ItemID,{-Pos-}Int)] = (\(_,q) -> q>0) `filter` iqlist
  iqplist <- billItems iqlist2
  return iqplist

addToCart :: CartID -> ItemID -> Int -> CSN [(ItemID,{-Pos-}Int,Int)]
addToCart cartID itemID qty = 
  atomically ($(checkTxn "_addToCartTxn" addToCartTxnCtrt)) $ do
    let key = mkKey cartID
    res::() <- invoke key AddItemsToCart (itemID,qty)
    summarizeCart cartID

removeFromCart :: CartID -> ItemID -> Int -> CSN [(ItemID,{-Pos-}Int,Int)]
removeFromCart cartID itemID qty = 
  atomically ($(checkTxn "_removeFromCartTxn" removeFromCartTxnCtrt)) $ do
    let key = mkKey cartID
    res::() <- invoke key RemoveItemsFromCart (itemID,qty)
    summarizeCart cartID

reviewCart :: CartID -> CSN ()
reviewCart cartID = do
    iqplist <- summarizeCart cartID
    liftIO $ putStrLn "User Reviewing the Cart ..."

processPayment :: CartID -> CSN ()
processPayment cartID = do
    iqplist <- summarizeCart cartID
    let bill = foldr (\(_,_,p) sum -> p+sum) 0 iqplist 
    liftIO $ putStrLn $ "Processing payment for $"++(show bill)++" ..."

checkOut :: CartID -> CSN ()
checkOut cartID = 
  atomically ($(checkTxn "_checkOutTxn" checkOutTxnCtrt)) $ do
    r::() <- reviewCart cartID
    processPayment cartID

