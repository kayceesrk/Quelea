{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module CounterDefs (
  Counter(..),
  createTables, dropTables,
  Operation(..),

  readCount, incCount,
  readCtrt, incCtrt
) where

import Database.Cassandra.CQL hiding (Counter)
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

data Counter = Inc_ | Read_
                 deriving (Eq, Read, Show)

$(derive makeSerialize ''Counter)

instance CasType Counter where
  getCas = get
  putCas = put
  casType _ = CBlob

readCount :: [Counter] -> () -> (Int, Maybe Counter)
readCount effs _ = (length effs, Nothing)

incCount :: [Counter] -> () -> ((), Maybe Counter)
incCount _ _ = ((), Just $ Inc_ )

instance Effectish Counter where
  summarize ctxt = ctxt

mkOperations [''Counter]
$(derive makeSerialize ''Operation)

incCtrt :: Contract Operation
incCtrt x = liftProp true

readCtrt :: Contract Operation
readCtrt x = forallQ2_ [Inc] [Read] $ \a b -> liftProp $ vis a b ∧ soo b x 
                                                          ⇒ vis a x 

--------------------------------------------------------------------------------

createTables :: Cas ()
createTables = do
  createTxnTable
  createTable "Counter"

dropTables :: Cas ()
dropTables = do
  dropTxnTable
  dropTable "Counter"
