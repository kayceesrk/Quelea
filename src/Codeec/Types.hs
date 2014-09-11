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
  Response(..),

  Key(..),
  Addr(..),
  SessUUID,
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
class (Enum a, Show a, Read a, Eq a, Ord a) => OperationClass a where
  getObjType :: a -> String

type AvailabilityMap a = Map (ObjType, a) (GenOpFun, Availability)
type DependenceMap a = Map a (S.Set a)

data DatatypeLibrary a = DatatypeLibrary {
  _avMap  :: AvailabilityMap a,
  _depMap :: DependenceMap a
}

newtype Key = Key { unKey :: UUID } deriving (Eq, Ord, Show)

type SessUUID = UUID
type SeqNo = Int64

data Request a = Request ObjType Key a ByteString SessUUID SeqNo

data Response = Response SeqNo ByteString

operationsTyConStr :: String
operationsTyConStr = "Operation"

data Addr = Addr {
  _sessid :: SessUUID,
  _seqno  :: SeqNo
} deriving (Eq, Ord, Read, Show)

instance CasType Addr where
  putCas (Addr x y) = do
    putLazyByteString $ toByteString x
    (putWord64be . fromIntegral) y
  getCas = do
    x <- fromJust . fromByteString <$> getLazyByteString 16
    y <- fromIntegral <$> getWord64be
    return $ Addr x y
  casType _ = CBlob
