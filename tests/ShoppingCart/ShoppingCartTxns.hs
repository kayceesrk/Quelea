{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module ShoppingCartTxns (
  openSite,
  searchItem,
  addToCart,
  removeFromCart,
  reviewCart,
  payAndCheckout
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

addToCart :: CartID -> ItemID -> Int -> CSN [(ItemID,Int)]
addToCart cartID itemID qty = 
  atomically ($(checkTxn "_addToCartTxn" addToCartTxnCtrt)) $ do
    let key = mkKey cartID
    res::() <- invoke key AddItemsToCart (itemID,qty)
    ilist::[(ItemID,Int)] <- invoke key GetItemsInCart ()
    return ilist

removeFromCart :: CartID -> ItemID -> Int -> CSN [(ItemID,Int)]
removeFromCart cartID itemID qty = 
  atomically ($(checkTxn "_removeFromCartTxn" removeFromCartTxnCtrt)) $ do
    let key = mkKey cartID
    res::() <- invoke key RemoveItemsFromCart (itemID,qty)
    ilist::[(ItemID,Int)] <- invoke key GetItemsInCart ()
    return ilist


inStock :: (ItemID,Int) -> CSN Bool
inStock (itemId,qty) = do
  (avlblQty::Int) <- invoke (mkKey itemId) ShowItem ()
  return (avlblQty >= qty)

{-reviewCart :: CartID -> CSN [(ItemID,Int)]
reviewCart cartID = 
  atomically ($(checkTxn "_reviewCartTxn" reviewCartTxnCtrt)) $ do
    let key = mkKey cartID
    ilist::[(ItemID,Int)] <- invoke key GetItemsInCart ()
    return $ map (\x -> if inStock x then x else error "Fail") ilist
-}

reviewCart :: ()
reviewCart = ()

payAndCheckout :: ()
payAndCheckout = ()

