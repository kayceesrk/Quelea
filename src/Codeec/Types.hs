{-# Language TemplateHaskell, EmptyDataDecls, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}

module Codeec.Types (
  Storable(..),
  Availability(..),
  DatatypeLibrary(..),
  GenOpFun(..),
  ObjType(..),
  OpFun(..),
  OperationClass(..),
  Request(..),

  operationsTyConStr
) where

import Database.Cassandra.CQL
import Data.Serialize as S
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Char8 (pack, unpack)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

class (CasType a, Serialize a) => Storable a where

type OpFun eff arg res = [eff] -> arg -> (res, Maybe eff)
type GenOpFun = [ByteString] -> ByteString -> (ByteString, Maybe ByteString)
data Availability = High | Sticky | Un deriving (Show, Eq, Ord)

instance Show GenOpFun where
  show f = "GenOpFun"

instance Lift Availability where
  lift High = [| High |]
  lift Sticky = [| Sticky |]
  lift Un = [| Un |]

type ObjType = String
class (Show a, Read a, Eq a, Ord a) => OperationClass a where
  getObjType :: a -> String

type DatatypeLibrary a = Map (ObjType, a) (GenOpFun, Availability)

data Request a = Request ObjType a ByteString

operationsTyConStr :: String
operationsTyConStr = "Operation"
