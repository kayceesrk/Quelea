{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module ShoppingCartCtrts (
  openSiteTxnCtrt,
  addToCartTxnCtrt,
  removeFromCartTxnCtrt,
  reviewCartTxnCtrt 
) where

import ShoppingCartDefs
import Codeec.Contract

openSiteTxnCtrt :: Fol Operation
openSiteTxnCtrt = liftProp $ true

addToCartTxnCtrt :: Fol Operation
addToCartTxnCtrt = liftProp $ true

removeFromCartTxnCtrt :: Fol Operation
removeFromCartTxnCtrt = liftProp $ true

reviewCartTxnCtrt :: Fol Operation
reviewCartTxnCtrt = liftProp $ true
