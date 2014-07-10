{-# Language TemplateHaskell, EmptyDataDecls, ScopedTypeVariables #-}

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
  OTC(..),

  mkRDT
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

class (CasType a, Serialize a) => Storable a where

type Operation eff arg res = [eff] -> arg -> (res, Maybe eff)
type GenericOperation = [ByteString] -> ByteString -> (ByteString, Maybe ByteString)
data Availability = High | Sticky | Un

class Show a => OTC a

newtype ObjType   = ObjType  { unObjType :: String } deriving (Eq, Ord)
newtype OperName  = OperName { unOperName :: String } deriving (Eq, Ord)
type Datatype = Map OperName (GenericOperation, Availability)
type DatatypeLibrary = Map ObjType Datatype

data Request = Request ObjType OperName ByteString

mkRDT :: Name -> Q [Dec]
mkRDT t = do
  TyConI (DataD _ (typeName::Name) _ constructors _) <- reify t
  let typeNameStr = nameBase typeName
  let t = take (length typeNameStr - 1) typeNameStr
  let consNameStrList = map (\ (NormalC name _) -> nameBase name) constructors
  let consList = map (\s -> take (length s - 1) s) consNameStrList
  d1 <- dataD (return []) (mkName t) [] [normalC (mkName t) []] [mkName "Show"]
  let ap = appT ([t| OTC |]) (conT $ mkName t)
  d2 <- instanceD (return []) ap []
  return $ [d1,d2]
