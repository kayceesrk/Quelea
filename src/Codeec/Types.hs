{-# Language TemplateHaskell #-}

module Codeec.Types (
  Storable(..),

  Availability(..),
  Datatype(..),
  DatatypeLibrary(..),
  GenericOperation(..),
  ObjType(..),
  Operation(..),
  OperName(..),
  Request(..),
) where

import Database.Cassandra.CQL
import Data.Serialize as S
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Char8 (pack, unpack)

class (CasType a, Serialize a) => Storable a where

type Operation eff arg res = [eff] -> arg -> (res, Maybe eff)
type GenericOperation = [ByteString] -> ByteString -> (ByteString, Maybe ByteString)
data Availability = High | Sticky | Un

newtype ObjType   = ObjType  { unObjType :: String } deriving (Eq, Ord)
newtype OperName  = OperName { unOperName :: String } deriving (Eq, Ord)
type Datatype = Map OperName (GenericOperation, Availability)
type DatatypeLibrary = Map ObjType Datatype

data Request = Request ObjType OperName ByteString

