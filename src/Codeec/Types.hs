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

newtype Key = Key { unKey :: UUID }

type SessUUID = UUID
type SeqNo = Int64

data Request a = Request ObjType Key a ByteString SessUUID SeqNo

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
