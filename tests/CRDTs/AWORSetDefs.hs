{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module AWORSetDefs (
  AWORSet(..),
  createTables, dropTables,
  Operation(..),

  addElement, remElement,
  addCtrt, remCtrt
) where

import Database.Cassandra.CQL hiding (AWORSet)
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

data AWORSet = Add_ Int | Rem_ Int | Display_
		deriving (Eq, Show)

$(derive makeSerialize ''AWORSet)

instance CasType AWORSet where
  getCas = get
  putCas = put
  casType _ = CBlob


addElement :: [AWOR] -> Int -> ((), Maybe AWOR)
addElement hist x = ((), Just $ Add_ x)

remElement :: [AWOR] -> Int -> ((), Maybe AWOR)
remElement hist x = if Data.Set.member x (displayELement hist ()) 
			then (True, Just $ Rem_ x)
			else (False, Nothing)

displayElement :: [AWOR] -> () -> ([Int], Maybe AWORSet)
displayElement hist _ = ((summ [] hist), Nothing)
	where
	  summ :: [Int] -> [AWORSet] -> [Int]
	  summ nset ((Add_ x):xs) = if Data.Set.member x nset
					then nset 
					else Data.Set.insert x nset
	  sum  nset ((Rem_ x):xs) = if Data.Set.member x nset
					then Data.Set.delete x nset
					else nset

instance Effectish AWORSet where
  summarize ctxt = ctxt

mkOperations [''AWORSet]
$(derive makeSerialize ''Operation)

addElement :: Contract Operation
addElement x = liftProp true

remElement :: Contract operation
remElement x = forallQ_ [Add] $ \a -> liftProp $
			(hbo a x -> vis a x) 

displayElement :: Contract operation
