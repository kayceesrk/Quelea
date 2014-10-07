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
  put (OperationPayload ot k on v sessid seqno mbtxnid) = do
    put ot
    put k
    put $ show on
    put v
    put sessid
    put seqno
    put mbtxnid
  get = do
    ot <- get
    k <- get
    on <- get
    v <- get
    sessid <- get
    seqno <- get
    mbtxnid <- get
    return $ OperationPayload ot k (read on) v sessid seqno mbtxnid

instance Serialize Key where
  put (Key k) = put k
  get = Key <$> get

instance Serialize SessID where
  put (SessID k) = put k
  get = SessID <$> get

instance Serialize TxnID where
  put (TxnID t) = put t
  get = TxnID <$> get

instance CasType TxnID where
  putCas = put
  getCas = get
  casType _ = CBlob

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
  put (ResOper seqno res eff txns) = do
    put (0::Word8)
    put seqno
    put res
    put eff
    put txns
  put (ResSnapshot v) = do
    put (1::Word8)
    put v
  put ResCommit = put (2::Word8)
  get = do
    i::Word8 <- get
    case i of
      0 -> do
        seqno <- get
        res <- get
        eff <- get
        txns <- get
        return $ ResOper seqno res eff txns
      1 -> do
        v <- get
        return $ ResSnapshot v
      2 -> return ResCommit

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

instance Serialize Addr where
  put (Addr x y) = put x >> put y
  get = do
    x <- get
    y <- get
    return $ Addr x y

instance CasType Addr where
  putCas = put
  getCas = get
  casType _ = CBlob

instance Serialize TxnPayload where
  put (RC wb) = do
    put (0::Word8)
    put wb
  put (MAV wb deps) = do
    put (1::Word8)
    put wb
    put deps
  put (PSI snapshot) = do
    put (2::Word8)
    put snapshot
  get = do
    i::Word8 <- get
    case i of
      0 -> RC <$> get
      1 -> do
        x <- get
        y <- get
        return $ MAV x y
      2 -> PSI <$> get

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
  casType _ = CBlob

instance CasType Key where
  putCas = put
  getCas = get
  casType _ = CBlob

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
  put (ReqSnapshot s) = do
    putWord8 2
    put s
  get = do
    i <- getWord8
    case i of
      0 -> ReqOper <$> get
      1 -> do
        txid <- get
        dep <- get
        return $ ReqTxnCommit txid dep
      2 -> do
        s <- get
        return $ ReqSnapshot s
