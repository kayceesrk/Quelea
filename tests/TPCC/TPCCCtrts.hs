{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module TPCCCtrts (
  doNewOrderTxnCtrt,
  doPaymentTxnCtrt,
  doDeliveryTxnCtrt
) where

import TPCCDefs
import Quelea.Contract

{- Atomic : ∀a,b,c. txn(τ, {a,b}) ∧ ¬txn(τ, {c}) ∧ (vis(a,b) ∨ vis(c,b))
  ⇒ vis(a,b) ∧ vis(c,b) -}

doNewOrderTxnCtrt :: Fol Operation
doNewOrderTxnCtrt = liftProp $ true

doPaymentTxnCtrt :: Fol Operation
doPaymentTxnCtrt = liftProp $ true

doDeliveryTxnCtrt :: Fol Operation
doDeliveryTxnCtrt = liftProp $ true