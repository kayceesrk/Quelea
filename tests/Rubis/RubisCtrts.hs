{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module RubisCtrts (
  bidForItemTxnCtrt,
  showMyBidsTxnCtrt,
  cancelBidTxnCtrt,
  openAuctionTxnCtrt,
  showMyAuctionsTxnCtrt,
  concludeAuctionTxnCtrt
) where

import RubisDefs
import Codeec.Contract

bidForItemTxnCtrt :: Fol Operation
bidForItemTxnCtrt = liftProp $ true

showMyBidsTxnCtrt :: Fol Operation
showMyBidsTxnCtrt = liftProp $ true

-- change to MAV
cancelBidTxnCtrt :: Fol Operation
cancelBidTxnCtrt = liftProp $ true

openAuctionTxnCtrt :: Fol Operation
openAuctionTxnCtrt = liftProp $ true

showMyAuctionsTxnCtrt :: Fol Operation
showMyAuctionsTxnCtrt = liftProp $ true

-- change to MAV
concludeAuctionTxnCtrt :: Fol Operation
concludeAuctionTxnCtrt = liftProp $ true
