{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module LWWRegisterDefs (
  LWWRegister(..),
  readReg, writeReg,
  haReadCtrt, haWriteCtrt,
  cauReadCtrt, cauWriteCtrt,
  stReadCtrt, stWriteCtrt,
  Operation(..),
  createTables, dropTables
) where

import Database.Cassandra.CQL as CQL hiding (WriteReg,write,Read,read)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize as S
import Data.Word (Word8)
import Control.Applicative ((<$>))
import Quelea.Types
import Quelea.Contract
import Quelea.TH
import Quelea.DBDriver
import Debug.Trace
import Data.DeriveTH
import Data.Time.Clock
import Data.List (sortBy)

data LWWRegister = WriteReg_ (Maybe UTCTime) Int
                 | HAWrite_ Int
                 | HARead_
                 | CAUWrite_ Int
                 | CAURead_
                 | STWrite_ Int
                 | STRead_
                 deriving (Eq, Read, Show)

instance Ord LWWRegister where
  (<=) (WriteReg_ ts1 v1) (WriteReg_ ts2 v2) =
    case (ts1, ts2) of
      (Nothing, Nothing) -> True
      (Nothing, Just _) -> True
      (Just _, Nothing) -> False
      (Just t1, Just t2) -> t1 <= t2

$(derive makeSerialize ''LWWRegister)

instance CasType LWWRegister where
  getCas = get
  putCas = put
  casType _ = CBlob

readReg :: [LWWRegister] -> () -> (Int, Maybe LWWRegister)
readReg effs _ =
  let ((WriteReg_ _ v):_) = summarize effs
  in (v, Nothing)

writeReg :: [LWWRegister] -> (UTCTime, Int) -> ((), Maybe LWWRegister)
writeReg _ (ts,v) = ((), Just $ WriteReg_ (Just ts) v)

flipRes :: Ordering -> Ordering
flipRes EQ = EQ
flipRes LT = GT
flipRes GT = LT

instance Effectish LWWRegister where
  summarize ctxt =
    case sortBy (\a b -> flipRes $ compare a b) ctxt of
      [] -> [WriteReg_ Nothing 0]
      x:_ -> [x]

mkOperations [''LWWRegister]
$(derive makeSerialize ''Operation)

haReadCtrt :: Contract Operation
haReadCtrt x = liftProp $ true

haWriteCtrt :: Contract Operation
haWriteCtrt x = liftProp $ true

cauReadCtrt :: Contract Operation
cauReadCtrt x = forallQ_ [WriteReg] $ \a -> liftProp $ hbo a x ⇒ vis a x

cauWriteCtrt :: Contract Operation
cauWriteCtrt x = forallQ_ [WriteReg] $ \a -> liftProp $ hbo a x ⇒ vis a x

stReadCtrt :: Contract Operation
stReadCtrt x = forallQ_ [WriteReg] $ \a -> liftProp $ vis a x ∨ vis x a ∨ sameEff a x

stWriteCtrt :: Contract Operation
stWriteCtrt x = forallQ_ [WriteReg] $ \a -> liftProp $ vis a x ∨ vis x a ∨ sameEff a x

--------------------------------------------------------------------------------

createTables :: Cas ()
createTables = do
  createTxnTable
  createTable "LWWRegister"

dropTables :: Cas ()
dropTables = do
  dropTxnTable
  dropTable "LWWRegister"
