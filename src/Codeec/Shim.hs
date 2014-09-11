{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell, DataKinds, OverloadedStrings  #-}

module Codeec.Shim (
 runShimNode,
 mkDtLib
) where

import Codeec.Types
import Codeec.NameService.SimpleBroker
import Codeec.Marshall
import Codeec.DBDriver
import Codeec.ShimLayer.Cache
import Codeec.Contract.Language

import Data.Serialize
import Control.Applicative ((<$>))
import Control.Monad (forever)
import Data.ByteString hiding (map, pack, putStrLn)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as M
import System.ZMQ4
import Data.Maybe (fromJust)
import Control.Lens
import Database.Cassandra.CQL
import Data.UUID
import Data.Int (Int64)
import qualified Data.Set as S
import Data.Text hiding (map)
import Debug.Trace

makeLenses ''Addr
makeLenses ''DatatypeLibrary

runShimNode :: OperationClass a
            => DatatypeLibrary a
            -> [Server] -> Keyspace -- Cassandra connection info
            -> Backend -> Int       -- Shim layer broker connection info
            -> IO ()
runShimNode dtLib serverList keyspace backend port = do

  {- ZeroMQ connection to the Shim layer broker. This lets the shim node talk
   - to clients. -}
  ctxt <- context
  sock <- socket ctxt Rep
  let myaddr = "tcp://*:" ++ show port
  bind sock myaddr
  serverJoin backend $ "tcp://localhost:" ++ show port

  {- Connection to the Cassandra deployment -}
  pool <- newPool serverList keyspace Nothing

  {- Spawn cache manager -}
  cache <- initCacheManager pool

  {- loop forver servicing clients -}
  forever $ do
    req <- receive sock
    result <- doOp dtLib cache pool $ decodeRequest req
    send sock [] $ encode result

doOp :: OperationClass a => DatatypeLibrary a -> CacheManager -> Pool -> Request a -> IO Response
doOp dtLib cache pool request = do
  let (Request objType key operName arg sessid seqno) = request
  {- Fetch the operation from the datatype library using the object type and
  - operation name. -}
  let (op,_) = fromJust $ dtLib ^. avMap ^.at (objType, operName)
  -- Fetch the current context
  ctxtSet <- getContext cache objType key
  let (ctxt, deps) = S.foldl (\(ctxt,deps) (sid,sqn,eff) ->
        (eff:ctxt, M.insertWith S.union sid (S.singleton sqn) deps)) ([],M.empty) ctxtSet
  let (res, effM) = op ctxt arg
  -- Add current location to the ones for which updates will be fetched
  addHotLocation cache objType key
  case effM of
    Nothing -> return $ Response seqno res
    Just eff -> do
      -- Write to database
      -- TODO: Calculate nearest dependencies
      runCas pool $ cqlWrite objType key (sessid, seqno + 1, S.fromList [Addr sessid 0], eff)
      -- Add effect to cache
      addEffectToCache cache objType key sessid (seqno+1) eff
      -- Return response
      return $ Response (seqno + 1) res

mkDtLib :: OperationClass a => [(a, GenOpFun, Availability, Contract a)] -> DatatypeLibrary a
mkDtLib l = DatatypeLibrary $ Prelude.foldl core M.empty l
  where
    core dtlib (op,fun,av,_) = M.insert (getObjType op, op) (fun, av) dtlib
