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

type ObjType = String
class (Show a, Read a, Eq a, Ord a) => OperName a where
  getObjType :: a -> String

type Datatype a = Map a (GenericOperation, Availability)
type DatatypeLibrary a = Map ObjType (Datatype a)

data Request a = Request ObjType a ByteString

mkRDT :: Name -> Q [Dec]
mkRDT t = do
  TyConI (DataD _ (typeName::Name) _ constructors _) <- reify t
  let typeNameStr = nameBase typeName
  let t = take (length typeNameStr - 1) typeNameStr
  let t_ = typeNameStr ++ "_"
  let consNameStrList = map (\ (NormalC name _) -> nameBase name) constructors
  let consList = map (\s -> take (length s - 1) s) consNameStrList
  let consNameList = map (\s -> normalC (mkName s) []) consList

  d1 <- dataD (return []) (mkName t) [] [normalC (mkName t) []] [mkName "Show", mkName "Eq", mkName "Ord", mkName "Read"]
  let ap = appT ([t| ObjType |]) (conT $ mkName t)
  d2 <- instanceD (return []) ap []

  d3 <- dataD (return []) (mkName t_) [] consNameList [mkName "Show", mkName "Eq", mkName "Ord", mkName "Read"]
  let ap = appT ([t| OperName |]) (conT $ mkName t_)
  d4 <- instanceD (return []) ap []
  return $ [d1,d2,d3,d4]
