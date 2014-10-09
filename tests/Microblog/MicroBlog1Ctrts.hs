{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module MicroBlog1Ctrts (
  addNewUserTxnCtrt
) where

import MicroBlogDefs
import Codeec.Contract

addNewUserTxnCtrt :: Fol Operation
addNewUserTxnCtrt = forallQ_ [AddUsername] $ \a -> forallQ_ [AddUser] $ \b -> forall_ $ \c -> undefined
