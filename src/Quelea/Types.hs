{-# Language TemplateHaskell, EmptyDataDecls, ScopedTypeVariables,
    TypeSynonymInstances, FlexibleInstances #-}

module Quelea.Types (
  Cell(..),
  Deps(..),
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
  TxnDepSet(..),
  GCSetting(..),
  SeqNo,
  knownUUID,

  operationsTyConStr
) where

import Database.Cassandra.CQL
import Data.Serialize as S
import Control.Applicative ((<$>))
import Data.ByteString (ByteString, head)
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
import Data.Time

class (CasType a, Serialize a) => Effectish a where
  summarize :: [a] -> [a]

type OpFun eff arg res = [eff] -> arg -> (res, Maybe eff)
type GenOpFun = [ByteString] -> ByteString -> (ByteString, Maybe ByteString)
type GenSumFun = [ByteString] -> [ByteString]
data Availability = Eventual | Causal | Strong deriving (Show, Eq, Ord)

data TxnKind = RC
             | MAV
             | RR deriving (Show, Eq, Ord, Read)

instance Show GenOpFun where
  show f = "GenOpFun"

instance Lift TxnKind where
  lift RC = [| RC |]
  lift MAV = [| MAV |]
  lift RR = [| RR |]

instance Lift Availability where
  lift Eventual = [| Eventual |]
  lift Causal = [| Causal |]
  lift Strong = [| Strong |]

type ObjType = String
class (Show a, Read a, Eq a, Ord a, Serialize a) => OperationClass a where
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

newtype Key = Key { unKey :: ByteString } deriving (Eq, Ord)

instance Show Key where
  show (Key kv) = "Key " ++ (show $ Data.ByteString.head kv)


type SeqNo = Int64

newtype TxnID = TxnID { unTxnID :: UUID } deriving (Eq, Ord)

instance Show TxnID where
  show (TxnID uuid) = "TxnID " ++ (show . sel1 . toWords $ uuid)

newtype SessID = SessID { unSessID :: UUID } deriving (Eq, Ord)

instance Show SessID where
  show (SessID uuid) = "SessID " ++ (show . sel1 . toWords $ uuid)

type EffectVal = ByteString

data TxnPayload = RC_TxnPl  {writeBuffer :: S.Set (Addr, EffectVal)}
                | MAV_TxnPl {writeBuffer :: S.Set (Addr, EffectVal),
                       txnDepsMAV :: S.Set TxnID}
                | RR_TxnPl {cacheSI :: S.Set (Addr, EffectVal)}

data OperationPayload a = OperationPayload {
  _objTypeReq :: ObjType,
  _keyReq     :: Key,
  _opReq      :: a,
  _valReq     :: ByteString,
  _sidReq     :: SessID,
  _sqnReq     :: SeqNo,
  _txnReq     :: Maybe (TxnID, TxnPayload),
  _getDepsReq :: Bool
}

data Request a =
    ReqOper (OperationPayload a)
  | ReqTxnCommit TxnID (S.Set TxnDep)
  | ReqSnapshot (S.Set (ObjType,Key))

data Response = ResOper {
                  seqno  :: SeqNo, {- if an effect was produce (effect = Just _),
                                      then seqno is the sequence number of this
                                      effect. Otherwise, it is the seq no passed
                                      in the corresponding Request (See OperationPayload) -}
                  result :: ByteString,
                  effect :: Maybe EffectVal, {- Only relevant for RC, MAV and RR transactions -}
                  coveredTxns :: Maybe (S.Set TxnID), {- Only relevant for MAV transactions -}
                  visibilitySet :: S.Set Addr }
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

newtype TxnDepSet = TxnDepSet (S.Set TxnDep) deriving (Show, Eq)

-- The type of value stored in a row of the cassandra table
data Cell = EffectVal ByteString -- An effect value
          | GCMarker UTCTime     -- Marks a GC with GC start time
          deriving (Show, Eq)

newtype Deps = Deps (S.Set Addr) deriving (Show, Eq)

{- TODO: GCMarker should include a set of transaction identifiers corresponding
 - to the transactions to which the GC'ed effects belonged to. Otherwise, do
 - not GC effects that belong to a transaction.
 -}

knownUUID :: UUID
knownUUID = fromJust $ fromString $ "123e4567-e89b-12d3-a456-426655440000"

data GCSetting = No_GC | GC_Mem_Only | GC_Full deriving (Read, Show)
