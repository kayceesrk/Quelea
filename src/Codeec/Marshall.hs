{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}

module Codeec.Marshall (
  mkGenOp,
  mkGenSum,
  decodeOperationPayload,
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

decodeOperationPayload :: OperationClass a => ByteString -> Request a
decodeOperationPayload b =
  case decode b of
    Left s -> error $ "decodeOperationPayload : " ++ s
    Right v -> v

instance Serialize UUID where
  put = putLazyByteString . toByteString
  get = fromJust . fromByteString <$> getLazyByteString 16

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
    put x
    (putWord64be . fromIntegral) y
  getCas = do
    x <- get
    y <- fromIntegral <$> getWord64be
    return $ Addr x y
  casType _ = CBlob

instance CasType TxnDep where
  putCas (TxnDep ot (Key k) sid sqn) = do
    let otbs = pack ot
    putWord32be $ fromIntegral . Data.ByteString.length $ otbs
    putByteString otbs
    put k
    put sid
    putWord64be . fromIntegral $ sqn
  getCas = do
    length <- fromIntegral <$> getWord32be
    otbs <- getByteString length
    k <- get
    sid <- get
    sqn <- fromIntegral <$> getWord64be
    return $ TxnDep (unpack otbs) (Key k) sid sqn

instance Serialize TxnDep where
  put = putCas
  get = getCas

instance OperationClass a => Serialize (Request a) where
  put (ReqOper op) = do
    putWord8 0
    put op
  put (ReqTxnCommit txid dep) = do
    putWord8 1
    put txid
    put dep
  get = do
    i <- getWord8
    case i of
      0 -> ReqOper <$> get
      1 -> do
        txid <- get
        dep <- get
        return $ ReqTxnCommit txid dep
