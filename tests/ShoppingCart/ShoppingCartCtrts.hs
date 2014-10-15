{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module ShoppingCartCtrts (
  openSiteTxnCtrt,
  addToCartTxnCtrt,
  removeFromCartTxnCtrt,
  checkOutTxnCtrt
) where

import ShoppingCartDefs
import Codeec.Contract

openSiteTxnCtrt :: Fol Operation
openSiteTxnCtrt = liftProp $ true

addToCartTxnCtrt :: Fol Operation
addToCartTxnCtrt = liftProp $ true

removeFromCartTxnCtrt :: Fol Operation
removeFromCartTxnCtrt = liftProp $ true

checkOutTxnCtrt :: Fol Operation
checkOutTxnCtrt = forallQ3_ [AlterPrice,StockItem] [ShowItem] [ShowItem] $ 
                    \a b c -> liftProp $  
                        trans (Single a) (SameTxn b c) ∧ sameObj a b ∧ sameObj b c ∧ vis a b ⇒ vis a c
