{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module GSetDefs (
  GSet(..),
  createTables, dropTables,
  Operation(..),

  addElement, displaySet,
  addCtrt, displayCtrt
) where

import Database.Cassandra.CQL hiding (GSet)
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


data GSet = Add_ String | Display_ 
		deriving (Eq, Read, Show)

$(derive makeSerialize ''GSet)

instance CasType GSet where
  getCas = get
  putCas = put
  casType _ = CBlob

addElement :: [GSet] -> String -> ((), Maybe GSet)
addElement _ str = ((), Just $ Add_ str)

displaySet :: [GSet] -> () -> ([GSet], Maybe GSet)
displaySet hist () = ((Data.Set.fromList hist), Nothing)

instance Effectish GSet where
  summarize ctxt = ctxt

mkOperations [''GSet]
$(derive makeSerialize ''Operation)

addElement :: Contract Operation
addElement x = liftProp true

displayElement :: Contract operation
displayElement x = liftProp true


--------------------------------------------------------------------------------

createTables :: Cas ()
createTables = do
  createTxnTable
  createTable "GSet"

dropTables :: Cas ()
dropTables = do
  dropTxnTable
  dropTable "GSet"


--forallQ_ [Add] $ \a -> forallQ_ [Add] $ \b -> forallQ_ [Add] $ \c ->liftProp $
--			((vis a b ^ vis b x) -> vis a x) ^ 
--			(soo c x -> vis c x)
