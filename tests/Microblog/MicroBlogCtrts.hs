{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module MicroBlogCtrts (
  addNewUserTxnCtrt,
  getPasswordTxnCtrt,
  followUserTxnCtrt
) where

import MicroBlogDefs
import Codeec.Contract

addNewUserTxnCtrt :: Fol Operation
addNewUserTxnCtrt = liftProp $ true

getPasswordTxnCtrt :: Fol Operation
getPasswordTxnCtrt = forallQ4_ [GetUserID] [GetUserInfo] [AddUsername] [AddUser] $ \a b c d -> liftProp $
                       trans[[a,b],[c,d]] ∧ so a b ∧ vis c a ∧ sameObj b d ⇒ vis d b

followUserTxnCtrt :: Fol Operation
followUserTxnCtrt = liftProp $ true
