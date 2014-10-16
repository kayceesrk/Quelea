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

-- If a CancelBid operation in this transaction succeeds (i.e., sees
-- corresponding AddBid in a different transaction T'), then references
-- to this bid from other tables should also be removed (i.e,
-- RemoveWalletBid and RemoveItemBid must see AddWalletBid and
-- AddItemBid from transaction T')
cancelBidTxnCtrt :: Fol Operation
cancelBidTxnCtrt = forallQ4_ [CancelBid] [RemoveItemBid, RemoveWalletBid]
                             [AddBid] [AddItemBid, AddWalletBid] 
                             $ \c d a b -> liftProp $ trans (SameTxn c d) (SameTxn a b) ∧
                                            so c d ∧ sameObj b d ∧ vis a c ⇒ vis b d

openAuctionTxnCtrt :: Fol Operation
openAuctionTxnCtrt = liftProp $ true

showMyAuctionsTxnCtrt :: Fol Operation
showMyAuctionsTxnCtrt = liftProp $ true

-- If withdraw in this transaction sees withdraw in a different
-- transaction, and if there is a CancelBid in that trans, then it
-- must be visible to GetBid in this trans.  Since withdraw always
-- sees another withdraw, AddBid in this
-- transaction always sees all CancelBids
concludeAuctionTxnCtrt :: Fol Operation
concludeAuctionTxnCtrt = forallQ4_ [WithdrawFromWallet] [GetBid] 
                                   [WithdrawFromWallet] [CancelBid]
                             $ \c d a b -> liftProp $ trans (SameTxn c d) (SameTxn a b) ∧
                                            so c d ∧ sameObj b d ∧ vis a c ⇒ vis b d
