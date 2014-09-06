{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell, DataKinds, OverloadedStrings  #-}

module Codeec.Shim (
 runShimNode,
 mkDtLib
) where

import Codeec.Types
import Codeec.NameService.SimpleBroker
import Codeec.Marshall
import Data.Serialize
import Control.Applicative ((<$>))
import Control.Monad (forever)
import Data.ByteString hiding (map, pack)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import System.ZMQ4
import Data.Maybe (fromJust)
import Control.Lens
import Database.Cassandra.CQL
import Data.UUID
import Data.Int (Int64)
import qualified Data.Set as S
import Data.Text hiding (map)
import Control.Monad.Trans (liftIO)

makeLenses ''Addr

--------------------------------------------------------------------------------
-- Cassandra Link Layer
--------------------------------------------------------------------------------

type TableName = String
type RowValue = (UUID, SessUUID, SeqNo, S.Set Addr, ByteString)

mkCreateTable :: TableName -> Query Schema () ()
mkCreateTable tname = query $ pack $ "create table " ++ tname ++ " (obj uuid, sess uuid, seqno bigint, dep set<blob>, value ascii, primary key (sess, seqno)) "

mkDropTable :: TableName -> Query Schema () ()
mkDropTable tname = query $ pack $ "drop table " ++ tname

mkInsert :: TableName -> Query Write RowValue ()
mkInsert tname = query $ pack $ "insert into " ++ tname ++ " (obj, sess, seqno, vis, value) values (?, ?, ?, ?, ?)"

mkRead :: TableName -> Query Rows (UUID) RowValue
mkRead tname = query $ pack $ "select obj, sess, seqno, vis, value from " ++ tname ++ " where key = ?"

cqlRead :: TableName -> Key -> Cas [RowValue]
cqlRead tname (Key k) = executeRows ONE (mkRead tname) k

cqlWrite :: TableName -> RowValue -> Cas ()
cqlWrite tname rv = executeWrite ONE (mkInsert tname) rv

createTable :: TableName -> Cas ()
createTable tname = liftIO . print =<< executeSchema ALL (mkCreateTable tname) ()

dropTable :: TableName -> Cas ()
dropTable tname = liftIO . print =<< executeSchema ALL (mkDropTable tname) ()

--------------------------------------------------------------------------------

runShimNode :: OperationClass a
            => DatatypeLibrary a
            -> [Server] -> Keyspace -- Cassandra connection info
            -> Backend -> Int       -- Shim layer broker connection info
            -> IO ()
runShimNode dtlib serverList keyspace backend port = do
  {- ZeroMQ connection to the Shim layer broker. This lets the shim node talk
   - to clients. -}
  ctxt <- context
  sock <- socket ctxt Rep
  let myaddr = "tcp://*:" ++ show port
  bind sock myaddr
  serverJoin backend $ "tcp://localhost:" ++ show port
  {- Connection to the Cassandra deployment -}
  pool <- newPool serverList keyspace
  {- loop forver servicing clients -}
  forever $ do
    req <- receive sock
    result <- performOp dtlib pool $ decodeRequest req
    send sock [] result
  where
    performOp dtLib pool (Request objType key operName arg sessid seqno) = runCas pool $ do
      let (op,_) = fromJust $ dtLib ^.at (objType, operName)
      rows <- cqlRead objType key
      let ctxt = map (\(_,_,_,_,v) -> v) rows
      let (res, eff) = op ctxt arg
      case eff of
        Nothing -> return $ encode (res, seqno)
        Just eff -> do
          cqlWrite objType (unKey key, sessid, seqno + 1, S.fromList [Addr sessid 0], eff)
          return $ encode (res, seqno + 1)

mkDtLib :: OperationClass a => [(a, GenOpFun, Availability)] -> DatatypeLibrary a
mkDtLib l = Prelude.foldl core Map.empty l
  where
    core dtlib (op,fun,av) = Map.insert (getObjType op, op) (fun, av) dtlib
