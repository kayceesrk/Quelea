{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module RWORSetDefs (
  RWORSet(..),
  createTables, dropTables,
  Operation(..),

  addElement, remElement,
  addCtrt, remCtrt
) where

import Database.Cassandra.CQL hiding (RWORSet)
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

data RWORSet = Add_ Int | Rem_ Int | Display_
		deriving (Eq, Show)

$(derive makeSerialize ''RWORSet)

instance CasType RWORSet where
  getCas = get
  putCas = put
  casType _ = CBlob


addElement :: [RWOR] -> Int -> ((), Maybe RWOR)
addElement hist x = ((), Just $ Add_ x)

remElement :: [RWOR] -> Int -> ((), Maybe RWOR)
remElement hist x = if Data.Set.member x (displayELement hist ()) 
			then (True, Just $ Rem_ x)
			else (False, Nothing)

displayElement :: [RWOR] -> () -> ([Int], Maybe RWORSet)
displayElement hist _ = ((summ [] hist), Nothing)
	where
	  summ :: [Int] -> [RWORSet] -> [Int]
	  summ nset ((Add_ x):xs) = if Data.Set.member x nset
					then nset 
					else Data.Set.insert x nset
	  sum  nset ((Rem_ x):xs) = if Data.Set.member x nset
					then Data.Set.delete x nset
					else nset

addElement :: Contract Operation
addElement x = liftProp true

remElement :: Contract operation
remElement x = forallQ_ [Add] $ \a -> liftProp $
			(sameobj a x -> vis a x)


