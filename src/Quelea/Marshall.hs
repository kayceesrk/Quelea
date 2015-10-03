{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, TemplateHaskell #-}

module Quelea.Marshall (
  mkGenOp,
  mkGenSum,
  decodeOperationPayload,
  decodeResponse
) where

import Quelea.Types
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
import Data.DeriveTH
import Data.Time

$(derive makeSerialize ''OperationPayload)

$(derive makeSerialize ''Key)

$(derive makeSerialize ''SessID)

$(derive makeSerialize ''TxnID)

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
  get = do
    r <- fromByteString <$> getLazyByteString 16
    case r of
      Nothing -> error "serialize UUID"
      Just x -> return x

$(derive makeSerialize ''Response)

decodeResponse :: ByteString -> Response
decodeResponse b = case decode b of
                     Left s -> error $ "decodeResponse : " ++ s
                     Right v -> v

instance Serialize UTCTime where
  put = putCas
  get = getCas

$(derive makeSerialize ''Cell)

instance CasType Cell where
  putCas = put
  getCas = get
  casType _ = CBlob

{-
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
-}

$(derive makeSerialize ''Addr)

instance CasType Addr where
  putCas = put
  getCas = get
  casType _ = CBlob


$(derive makeSerialize ''Deps)

instance CasType Deps where
  putCas = put
  getCas = get
  casType _ = CBlob

$(derive makeSerialize ''TxnPayload)

$(derive makeSerialize ''TxnDep)

instance CasType TxnDep where
  putCas = put
  getCas = get
  casType _ = CBlob

$(derive makeSerialize ''TxnDepSet)

instance CasType TxnDepSet where
  putCas = put
  getCas = get
  casType _ = CBlob

instance CasType Key where
  putCas = put
  getCas = get
  casType _ = CBlob

$(derive makeSerialize ''Request)
