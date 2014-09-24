{-# LANGUAGE ScopedTypeVariables #-}

module Codeec.Marshall (
  mkGenOp,
  mkGenSum,
  decodeRequest,
  decodeResponse
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
import Data.UUID
import Data.Maybe (fromJust)


instance OperationClass a => Serialize (Request a) where
  put (Request ot (Key k) on v sessid seqno) = S.put (pack $ show ot, toByteString k, pack $ show on, v, toByteString sessid, seqno)
  get = do
    (s1,k,s2,v,sid,sqn) <- S.get
    return $ Request (read $ unpack s1) (Key $ fromJust $ fromByteString k) (read $ unpack s2) v
      (fromJust $ fromByteString sid) sqn

mkGenOp :: (Effectish eff, Serialize arg, Serialize res)
          => OpFun eff arg res
          -> ([eff] -> [eff])
          -> (GenOpFun, GenSumFun)
mkGenOp foo bar = (fun1 foo, mkGenSum bar)
  where
    fun1 foo ctxt arg =
      let ctxt2 = rights $ map decode ctxt
          arg2 = case decode arg of
                  Right v -> v
                  Left s -> error ("mkGenOp : " ++ s)
          (res, eff) = foo ctxt2 arg2
      in (encode res, encode <$> eff)

mkGenSum :: Effectish eff => ([eff] -> [eff]) -> GenSumFun
mkGenSum foo ctxt =
  let ctxt2 = rights $ map decode ctxt
      ctxt3 = foo ctxt2
  in encode <$> ctxt3

decodeRequest :: OperationClass a => ByteString -> Request a
decodeRequest b = case decode b of
                    Left s -> error $ "decodeRequest : " ++ s
                    Right v -> v


instance Serialize Response where
  put (Response seqno res) = S.put (seqno, res)
  get = do
    (a,b) <- S.get
    return $ Response a b

decodeResponse :: ByteString -> Response
decodeResponse b = case decode b of
                     Left s -> error $ "decodeResponse : " ++ s
                     Right v -> v
