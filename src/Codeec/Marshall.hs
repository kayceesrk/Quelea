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


instance OperName a => Serialize (Request a) where
  put (Request ot on v) = S.put (pack $ show ot, pack $ show on, v)
  get = do
    (s1,s2,v) <- S.get
    return $ Request (read $ unpack s1) (read $ unpack s2) v

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

decodeRequest :: OperName a => ByteString -> Request a
decodeRequest b = case decode b of
                    Left s -> error $ "decodeRequest : " ++ s
                    Right v -> v
