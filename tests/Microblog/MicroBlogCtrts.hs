{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module MicroBlogCtrts (
  addNewUserTxnCtrt,
  getPasswordTxnCtrt,
  followUserTxnCtrt,
  unfollowUserTxnCtrt,
  addToUserlineTxnCtrt,
  addToTimelineTxnCtrt,
  getUserlineTxnCtrt,
  getTimelineTxnCtrt,
  blockUserTxnCtrt
) where

import MicroBlogDefs
import Codeec.Contract

addNewUserTxnCtrt :: Fol Operation
addNewUserTxnCtrt = liftProp $ true

getPasswordTxnCtrt :: Fol Operation
getPasswordTxnCtrt = forallQ4_ [GetUserID] [GetUserInfo] [AddUsername] [AddUser] $ \a b c d -> liftProp $
                       trans (SameTxn a b) (SameTxn c d) ∧ so a b ∧ vis c a ∧ sameObj b d ⇒ vis d b

followUserTxnCtrt :: Fol Operation
followUserTxnCtrt = forallQ4_ [AddFollower] [AddFollowing] [Blocks] [IsBlockedBy] $ \a b c d -> liftProp $
                      trans (SameTxn a b) (SameTxn c d) ∧ vis d b ∧ sameObj a c ⇒ vis c a

unfollowUserTxnCtrt :: Fol Operation
unfollowUserTxnCtrt = liftProp $ true

blockUserTxnCtrt :: Fol Operation
blockUserTxnCtrt = liftProp $ true

addToUserlineTxnCtrt :: Fol Operation
addToUserlineTxnCtrt = liftProp $ true

addToTimelineTxnCtrt :: Fol Operation
addToTimelineTxnCtrt = liftProp $ true

getUserlineTxnCtrt :: Fol Operation
getUserlineTxnCtrt = forallQ4_ [GetTweetsInUL] [GetTweet] [NewTweet] [NewTweetUL] $ \a b c d -> liftProp $
                       trans (SameTxn a b) (DirDep c d) ∧ so a b ∧ vis d a ∧ sameObj b c ⇒ vis c b

getTimelineTxnCtrt :: Fol Operation
getTimelineTxnCtrt = forallQ4_ [GetTweetsInTL] [GetTweet] [NewTweet] [NewTweetTL] $ \a b c d -> liftProp $
                       trans (SameTxn a b) (DirDep c d) ∧ so a b ∧ vis d a ∧ sameObj b c ⇒ vis c b
