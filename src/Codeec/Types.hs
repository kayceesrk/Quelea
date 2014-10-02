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
  TxnPayload(..),
  TxnKind(..),

  Key(..),
  Addr(..),
  SessID(..),
  TxnID(..),
  TxnDep(..),
  SeqNo,
  knownUUID,

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
import Data.UUID hiding (show)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Tuple.Select (sel1)

class (CasType a, Serialize a) => Effectish a where
  summarize :: [a] -> [a]

type OpFun eff arg res = [eff] -> arg -> (res, Maybe eff)
type GenOpFun = [ByteString] -> ByteString -> (ByteString, Maybe ByteString)
type GenSumFun = [ByteString] -> [ByteString]
data Availability = High | Sticky | Un deriving (Show, Eq, Ord)

data TxnKind = ReadCommitted
             | MonotonicAtomicView
             | ParallelSnapshotIsolation
             | Serializability deriving (Show, Eq, Ord)

instance Show GenOpFun where
  show f = "GenOpFun"

instance Lift Availability where
  lift High = [| High |]
  lift Sticky = [| Sticky |]
  lift Un = [| Un |]

type ObjType = String
class (Show a, Read a, Eq a, Ord a) => OperationClass a where
  getObjType :: a -> String

instance OperationClass () where
  getObjType _ = fail "requesting ObjType of ()"

type AvailabilityMap a = Map (ObjType, a) (GenOpFun, Availability)
type SummaryMap = Map ObjType ([ByteString] -> [ByteString])
type DependenceMap a = Map a (S.Set a)

data DatatypeLibrary a = DatatypeLibrary {
  _avMap  :: AvailabilityMap a,
  _sumMap :: SummaryMap
}

newtype Key = Key { unKey :: UUID } deriving (Eq, Ord)

instance Show Key where
  show (Key uuid) = "Key " ++ (show . sel1 . toWords $ uuid)


type SeqNo = Int64

newtype TxnID = TxnID { unTxnID :: UUID } deriving (Eq, Ord)

instance Show TxnID where
  show (TxnID uuid) = "TxnID " ++ (show . sel1 . toWords $ uuid)

newtype SessID = SessID { unSessID :: UUID } deriving (Eq, Ord)

instance Show SessID where
  show (SessID uuid) = "SessID " ++ (show . sel1 . toWords $ uuid)

type EffectVal = ByteString

data TxnPayload = RC  {writeBuffer :: S.Set (Addr, EffectVal)}
                | MAV {writeBuffer :: S.Set (Addr, EffectVal),
                       txnDepsMAV :: S.Set TxnID}
                | PSI {cacheSI :: S.Set (Addr, EffectVal)}
                | SER

data OperationPayload a = OperationPayload {
  _objTypeReq :: ObjType,
  _keyReq     :: Key,
  _opReq      :: a,
  _valReq     :: ByteString,
  _sidReq     :: SessID,
  _sqnReq     :: SeqNo,
  _txnReq     :: Maybe (TxnID, TxnPayload)
}

data Request a =
    ReqOper (OperationPayload a)
  | ReqTxnCommit TxnID (S.Set TxnDep)
  | ReqSnapshot (S.Set (ObjType,Key))

data Response = ResOper {
                  seqno :: SeqNo,
                  result :: ByteString,
                  effect :: Maybe EffectVal,  {- Only relevant for RC, MAV and PSI transactions -}
                  coveredTxns :: Maybe (S.Set TxnID)} {- Only relevant for MAV transactions -}
              | ResSnapshot (M.Map (ObjType, Key) (S.Set (Addr, EffectVal)))
              | ResCommit

operationsTyConStr :: String
operationsTyConStr = "Operation"

data Addr = Addr {
  _sessid :: SessID,
  _seqno  :: SeqNo
} deriving (Eq, Ord, Show)

data TxnDep = TxnDep {
  _objTypeTx :: ObjType,
  _keyTx     :: Key,
  _sidTx     :: SessID,
  _sqnTx     :: SeqNo
} deriving (Eq, Ord, Show)


-- The type of value stored in a row of the cassandra table
data Cell = EffectVal ByteString -- An effect value
          | GCMarker             -- Marks a GC
          deriving (Show, Eq)

{- TODO: GCMarker should include a set of transaction identifiers corresponding
 - to the transactions to which the GC'ed effects belonged to. Otherwise, do
 - not GC effects that belong to a transaction.
 -}

knownUUID :: UUID
knownUUID = fromJust $ fromString $ "123e4567-e89b-12d3-a456-426655440000"
