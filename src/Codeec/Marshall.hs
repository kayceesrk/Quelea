module Codeec.Marshall (
  mkGeneric,
  decodeRequest
) where

import Codeec.Types
import Database.Cassandra.CQL
import Data.Serialize as S
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Char8 (pack, unpack)


instance Serialize Request where
  put (Request (ObjType s1) (OperName s2) v) = S.put (pack s1, pack s2, v)
  get = do
    (s1,s2,v) <- S.get
    return $ Request (ObjType $ unpack s1) (OperName $ unpack s2) v

mkGeneric :: (Storable eff, Serialize arg, Serialize res)
          => Operation eff arg res
          -> GenericOperation
mkGeneric foo ctxt arg =
  let ctxt2 = rights $ map decode ctxt
      arg2 = case decode arg of
               Right v -> v
               Left s -> error ("mkGeneric : " ++ s)
      (res, eff) = foo ctxt2 arg2
  in (encode res, encode <$> eff)

decodeRequest :: ByteString -> Request
decodeRequest b = case decode b of
                    Left s -> error $ "decodeRequest : " ++ s
                    Right v -> v
