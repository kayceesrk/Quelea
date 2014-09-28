{-# Language TemplateHaskell, EmptyDataDecls, ScopedTypeVariables,
    TypeSynonymInstances, FlexibleInstances #-}

module Codeec.Types (
  Cell(..),
  Effectish(..),
  Availability(..),
  DatatypeLibrary(..),
  GenOpFun(..),
  GenSumFun(..),
  ObjType(..),
  OpFun(..),
  OperationClass(..),
  OperationPayload(..),
  Request(..),
  Response(..),

  Key(..),
  Addr(..),
  SessUUID,
  TxnID,
  TxnDep(..),
  SeqNo,

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
import Data.UUID
import Data.Int (Int64)
import Data.Maybe (fromJust)
import qualified Data.Set as S

class (CasType a, Serialize a) => Effectish a where
  summarize :: [a] -> [a]

type OpFun eff arg res = [eff] -> arg -> (res, Maybe eff)
type GenOpFun = [ByteString] -> ByteString -> (ByteString, Maybe ByteString)
type GenSumFun = [ByteString] -> [ByteString]
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

type AvailabilityMap a = Map (ObjType, a) (GenOpFun, Availability)
type SummaryMap = Map ObjType ([ByteString] -> [ByteString])
type DependenceMap a = Map a (S.Set a)

data DatatypeLibrary a = DatatypeLibrary {
  _avMap  :: AvailabilityMap a,
  _sumMap :: SummaryMap
}

newtype Key = Key { unKey :: UUID } deriving (Eq, Ord, Show)

type TxnID = UUID
type SessUUID = UUID
type SeqNo = Int64

data OperationPayload a = OperationPayload {
  _objTypeReq :: ObjType,
  _keyReq     :: Key,
  _opReq      :: a,
  _valReq     :: ByteString,
  _sidReq     :: SessUUID,
  _sqnReq     :: SeqNo
}

data Request a =
    ReqOper (OperationPayload a)
  | ReqTxnCommit TxnID (S.Set TxnDep)

data Response = Response SeqNo ByteString

operationsTyConStr :: String
operationsTyConStr = "Operation"

data Addr = Addr {
  _sessid :: SessUUID,
  _seqno  :: SeqNo
} deriving (Eq, Ord, Read, Show)

data TxnDep = TxnDep {
  _objTypeTx :: ObjType,
  _keyTx     :: Key,
  _sidTx     :: SessUUID,
  _sqnTx     :: SeqNo
} deriving (Eq, Ord)

-- The type of value stored in a row of the cassandra table
data Cell = EffectVal ByteString -- An effect value
          | GCMarker             -- Marks a GC
          deriving Eq
