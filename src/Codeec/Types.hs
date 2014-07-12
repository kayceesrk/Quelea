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

mkRDT :: [Name] -> Q [Dec]
mkRDT l = do
  pl <- procNameList l
  let (_,consList) = unzip pl
  d1 <- dataD (return []) (mkName "Operations") [] consList [mkName "Show", mkName "Eq", mkName "Ord", mkName "Read"]
  let ap = appT ([t| OperName |]) (conT $ mkName "Operations")
  d2 <- instanceD (return []) ap [funD 'getObjType $ map mkGetObjType pl]
  return $ [d1,d2]
  where
    procNameList :: [Name] -> Q [(String,ConQ)]
    procNameList [] = return []
    procNameList (x:xs) = do
      TyConI (DataD _ (typeName::Name) _ constructors _) <- reify x
      let typeNameStr = nameBase typeName
      let consNameStrList = map (\ (NormalC name _) -> nameBase name) constructors
      let consList = map (\s -> normalC (mkName $ take (length s - 1) s) []) consNameStrList
      let pairList = map (\c -> (typeNameStr, c)) consList
      rest <- procNameList xs
      return $ pairList ++ rest

    mkGetObjType :: (String, ConQ) -> ClauseQ
    mkGetObjType (objType, con) = do
      NormalC conName _ <- con
      return $ Clause [ConP conName []] (NormalB (LitE (StringL objType))) []

