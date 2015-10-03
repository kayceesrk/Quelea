{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module BankAccountDefs (
  BankAccount(..),
  deposit, depositCtrt,
  withdraw, withdrawCtrt,
  getBalance, getBalanceCtrt,
  saveTxnCtrt, totalBalanceTxnCtrt,
  Operation(..),
  summarize
) where

import Database.Cassandra.CQL as CQL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize as S
import Data.Word (Word8)
import Control.Applicative ((<$>))
import Quelea.Types
import Quelea.Contract
import Quelea.TH
import Debug.Trace
import Data.DeriveTH

data BankAccount = Deposit_ Int | Withdraw_ Int | GetBalance_ deriving Show

$(derive makeSerialize ''BankAccount)

instance CasType BankAccount where
  getCas = get
  putCas = put
  casType _ = CBlob

type Res a = (a, Maybe BankAccount)

deposit :: [BankAccount] -> Int -> Res ()
deposit _ amt = ((), Just $ Deposit_ amt)


withdraw :: [BankAccount] -> Int -> Res Bool
withdraw ctxt amt =
  let (bal, _) = getBalance ctxt ()
  in if bal > amt
     then (True, Just $ Withdraw_ amt)
     else (False, Nothing)

getBalance :: [BankAccount] -> () -> Res Int
getBalance ops () =
  let v = {- trace ("GB : #effect=" ++ (show $ length ops)) -} foldl acc 0 ops
  in (v, Nothing)
  where
    acc s (Deposit_ i) = s + i
    acc s (Withdraw_ i) = s - i
    acc s GetBalance_ = s

instance Effectish BankAccount where
  summarize ctxt =
    let (v, _) = getBalance ctxt ()
    in [Deposit_ v]

mkOperations [''BankAccount]
$(derive makeSerialize ''Operation)

depositCtrt :: Contract Operation
depositCtrt x = liftProp $ true
-- depositCtrt x = forall_ $ \a -> liftProp $ hbo a x ⇒ vis a x

withdrawCtrt :: Contract Operation
withdrawCtrt x = forallQ_ [Deposit] $ \a -> forallQ_ [Withdraw] $ \b ->
                 forallQ_ [Withdraw] $ \c -> liftProp $
                    ((vis a b ∧ vis b x) ⇒ vis a x) ∧
                    (vis c x ∨ vis x c ∨ appRel SameEff x c)

getBalanceCtrt :: Contract Operation
getBalanceCtrt x = forallQ_ [Deposit] $ \a -> forallQ_ [Withdraw] $ \b -> liftProp $
                      (vis a b ∧ vis b x) ⇒ vis a x

--------------------------------------------------------------------------------

saveTxnCtrt :: Fol Operation
saveTxnCtrt = liftProp $ true

totalBalanceTxnCtrt :: Fol Operation
totalBalanceTxnCtrt = forallQ4_ [GetBalance] [GetBalance] [Deposit, Withdraw] [Deposit, Withdraw] $ \a b c d -> liftProp $
                        trans (SameTxn a b) (SameTxn c d) ∧ vis c a ∧ sameObj d b ⇒ vis d b
