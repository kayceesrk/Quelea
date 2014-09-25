{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}

module Codeec.Marshall (
  mkGenOp,
  mkGenSum,
  getRequestKind,
  decodeOperationPayload,
  decodeReqEndSess,
  decodeResponse
) where

import Codeec.Types
import Database.Cassandra.CQL
import Data.Serialize as S
import Control.Applicative ((<$>))
import Data.ByteString (ByteString, length, head, tail)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Char8 (pack, unpack)
import Data.UUID
import Data.Maybe (fromJust)
import Data.Word

instance OperationClass a => Serialize (OperationPayload a) where
  put (OperationPayload ot (Key k) on v sessid seqno) = S.put (pack $ show ot, toByteString k, pack $ show on, v, toByteString sessid, seqno)
  get = do
    (s1,k,s2,v,sid,sqn) <- S.get
    return $ OperationPayload (read $ unpack s1) (Key $ fromJust $ fromByteString k) (read $ unpack s2) v
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

getRequestKind :: ByteString -> Request
getRequestKind b =
  case Data.ByteString.head b of
    0 -> ReqOper
    1 -> ReqEndSess

decodeOperationPayload :: OperationClass a => ByteString -> OperationPayload a
decodeOperationPayload b =
  case decode $ Data.ByteString.tail b of
    Left s -> error $ "decodeOperationPayload : " ++ s
    Right v -> v

decodeReqEndSess :: ByteString -> SessUUID
decodeReqEndSess b =
  case decode $ Data.ByteString.tail b of
    Left s -> error $ "decodeReqEndSess : " ++ s
    Right v -> v

instance Serialize SessUUID where
  put sid = S.put $ toByteString sid
  get = do
    sid <- S.get
    return $ fromJust $ fromByteString sid

instance Serialize Response where
  put (Response seqno res) = S.put (seqno, res)
  get = do
    (a,b) <- S.get
    return $ Response a b

decodeResponse :: ByteString -> Response
decodeResponse b = case decode b of
                     Left s -> error $ "decodeResponse : " ++ s
                     Right v -> v

instance CasType Cell where
  putCas (EffectVal b) = do
    putWord8 0
    putWord32be $ fromIntegral $ Data.ByteString.length b
    putByteString b
  putCas GCMarker = putWord8 1
  getCas = do
    i <- getWord8
    case i of
      0 -> do
        l <- fromIntegral <$> getWord32be
        bs <- getByteString l
        return $ EffectVal bs
      1 -> return $ GCMarker
  casType _ = CBlob

instance CasType Addr where
  putCas (Addr x y) = do
    putLazyByteString $ toByteString x
    (putWord64be . fromIntegral) y
  getCas = do
    x <- fromJust . fromByteString <$> getLazyByteString 16
    y <- fromIntegral <$> getWord64be
    return $ Addr x y
  casType _ = CBlob

