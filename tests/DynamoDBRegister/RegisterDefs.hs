{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module RegisterDefs (
  Operation(..),
  putValue, putValueCtrt,
  getValue, getValueCtrt,
  condPutValue, condPutValueCtrt,
  strongGetValueCtrt,
  incValue, incValueCtrt,
  decValue, decValueCtrt,
  createTables, dropTables
) where


import Database.Cassandra.CQL
import Data.Serialize hiding (Put, Get)

import Codeec.Types
import Codeec.Contract
import Codeec.TH
import Codeec.DBDriver
import Data.Int (Int64)
import Control.Applicative ((<$>))
import Data.Tuple.Select (sel1)
import Data.DeriveTH

newtype Round = Round {unRound :: Int64} deriving (Eq, Show)
newtype Value = Value {unValue :: Int64} deriving (Eq, Show)

data Register = Put_ Round Value
              | CondPut_ Round Value
              | Get_
              | StrongGet_
              | Inc_ Round Value
              | Dec_ Round Value deriving Eq

$(derive makeSerialize ''Round)
$(derive makeSerialize ''Value)
$(derive makeSerialize ''Register)

instance CasType Register where
  putCas = put
  getCas = get
  casType _ = CBlob

instance Effectish Register where
  summarize effs =
    let (_,summary) =
          foldl (\(curRound, acc) eff ->
                    if getRound eff < curRound then (curRound, acc)
                    else if getRound eff > curRound then (getRound eff, [eff])
                    else (curRound, eff:acc)) (-1,[]) effs
    in summary

getRound :: Register -> Int64
getRound (Put_ r _) = unRound r
getRound (CondPut_ r _) = unRound r
getRound (Inc_ r _) = unRound r
getRound (Dec_ r _) = unRound r
getRound v | v == Get_ || v == StrongGet_ = -1

getMaxRound :: [Register] -> Int64
getMaxRound effList = foldl (\r e -> max r $ getRound e) (-1) effList

getValue :: [Register] -> () -> (Int64, Maybe Register)
getValue effList _ =
  let summary = effList
      value = foldl (\v eff -> case eff of
                                 Inc_ r x -> v + unValue x
                                 Dec_ r x -> v - unValue x
                                 Put_ r x -> v + unValue x
                                 CondPut_ r x -> v + unValue x) 0 summary
  in (value, Nothing)

putValue :: [Register] -> Int64 -> ((), Maybe Register)
putValue effList v = ((), Just $ Put_ (Round $ getMaxRound effList + 1) (Value v))

condPutValue :: [Register] -> (Int64, Int64) -> (Int64, Maybe Register)
condPutValue effList (oldValue, newValue) =
  let curValue = sel1 $ getValue effList ()
  in if curValue == oldValue
     then (curValue, Just $ CondPut_ (Round $ getMaxRound effList + 1) (Value newValue))
     else (curValue, Nothing)

incValue :: [Register] -> Int64 -> (Int64, Maybe Register)
incValue effList delta =
  let curValue = sel1 $ getValue effList ()
  in (curValue, Just $ Inc_ (Round $ getMaxRound effList) (Value delta))

decValue :: [Register] -> Int64 -> (Int64, Maybe Register)
decValue effList delta =
  let curValue = sel1 $ getValue effList ()
  in (curValue, Just $ Dec_ (Round $ getMaxRound effList) (Value delta))

mkOperations [''Register]
$(derive makeSerialize ''Operation)

getValueCtrt :: Contract Operation
getValueCtrt x = forall_ $ \a -> liftProp $ appRel ((^+) ((So ∩ SameObj) ∪ Vis)) a x ⇒ vis a x

putValueCtrt :: Contract Operation
putValueCtrt x = forallQ_ [Put, CondPut] $ \a -> liftProp $ sameObj a x ⇒ vis a x ∨ vis x a ∨ sameEff a x

condPutValueCtrt :: Contract Operation
condPutValueCtrt x = forallQ_ [Put, CondPut] $ \a -> liftProp $ sameObj a x ⇒ vis a x ∨ vis x a ∨ sameEff a x

strongGetValueCtrt :: Contract Operation
strongGetValueCtrt x = forallQ_ [Put, CondPut] $ \a -> liftProp $ sameObj a x ⇒ vis a x ∨ vis x a ∨ sameEff a x

incValueCtrt :: Contract Operation
incValueCtrt x = liftProp $ true

decValueCtrt :: Contract Operation
decValueCtrt x = liftProp $ true

--------------------------------------------------------------------------------

createTables :: Cas ()
createTables = do
  createTxnTable
  createTable "Register"

dropTables :: Cas ()
dropTables = do
  dropTxnTable
  dropTable "Register"
