{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module TwoPSetDefs (
  TwoPSet(..),
  createTables, dropTables,
  Operation(..),

  addElement, remElement,
  addCtrt, remCtrt
) where

import Database.Cassandra.CQL hiding (TwoPSet)
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


data TwoPSet = Add_ Int | Rem_ Int | Display_
		deriving (Eq, Show)

$(derive makeSerialize ''TwoPSet)

instance CasType TwoPSet where
  getCas = get
  putCas = put
  casType _ = CBlob

addElement :: [TwoPSet] -> Int -> ((Bool), Maybe TwoPSet)
addElement hist x = if chk (Rem_ x) hist 
			then (False, Nothing) 
			else (True, Just $ Add_ x)
	where
	  chk :: TwoPSet -> [TwoPSet] -> Bool
	  chk x hist = Data.Set.member x hist


remElement :: [TwoPSet] -> Int -> ((Bool), Maybe TwoPSet)
remElement hist x = if chk (Add_ x) hist 
			then (False, Nothing) 
			else (True, Just $ Add_ x)
	where
	  chk :: TwoPSet -> [TwoPSet] -> Bool
	  chk x hist = Data.Set.member x hist


displayElement :: [TwoPSet] -> () -> ([Int], Maybe TwoPSet)
displayElement hist _ = ((summ [] hist), Nothing)
	where
	  summ :: [Int] -> [TwoPSet] -> [Int]
	  summ nset ((Add_ x):xs) = if chk (Rem_ x) hist
					then nset
					else 
						if Data.Set.member x nset
							then nset 
							else Data.Set.insert x nset
	  summ  nset ((Rem_ x):xs) = if Data.Set.member x nset
					then Data.Set.delete x nset
	  				else nset
	  chk :: TwoPSet -> [TwoPSet] -> Bool
	  chk x hist = Data.Set.member x hist
	


instance Effectish TwoPSet where
  summarize ctxt = ctxt

mkOperations [''TwoPSet]
$(derive makeSerialize ''Operation)

addElement :: Contract Operation
addElement x = forallQ_ [Rem] $ \a -> liftProp $
			(hbo a x -> vis a x)

remElement :: Contract operation
remElement x = forallQ_ [Add] $ \a -> liftProp $
			(soo a x -> vis a x)

displayElement :: Contract operation
displayElement = liftProp true

