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
import Data.Text
import Control.Monad.Trans (liftIO)

makeLenses ''Addr

--------------------------------------------------------------------------------
-- Cassandra Link Layer
--------------------------------------------------------------------------------

type TableName = String
type RowValue = (UUID, UUID, Int64, S.Set Addr, ByteString)

mkCreateTable :: TableName -> Query Schema () ()
mkCreateTable tname = query $ pack $ "create table " ++ tname ++ " (obj uuid, sess uuid, seqno bigint, dep set<blob>, value ascii, primary key (sess, seqno)) "

mkDropTable :: TableName -> Query Schema () ()
mkDropTable tname = query $ pack $ "drop table " ++ tname

mkInsert :: TableName -> Query Write RowValue ()
mkInsert tname = query $ pack $ "insert into " ++ tname ++ " (obj, sess, seqno, vis, value) values (?, ?, ?, ?, ?)"

mkRead :: TableName -> Query Rows (Key) RowValue
mkRead tname = query $ pack $ "select obj, sess, seqno, vis, value from " ++ tname ++ " where key = ?"

createTable :: TableName -> Cas ()
createTable tname = liftIO . print =<< executeSchema ALL (mkCreateTable tname) ()

dropTable :: TableName -> Cas ()
dropTable tname = liftIO . print =<< executeSchema ALL (mkDropTable tname) ()

runShimNode :: OperationClass a
            => DatatypeLibrary a -> Backend -> Int -> IO ()
runShimNode dtlib backend port = do
  ctxt <- context
  sock <- socket ctxt Rep
  let myaddr = "tcp://*:" ++ show port
  bind sock myaddr
  serverJoin backend $ "tcp://localhost:" ++ show port
  forever $ do
    req <- receive sock
    result <- performOp dtlib $ decodeRequest req
    send sock [] result
  where
    performOp dtLib (Request objType key operName arg) =
      let (op,_) = fromJust $ dtLib ^.at (objType, operName)
          (res, eff) = op [] arg
      in return res

mkDtLib :: OperationClass a => [(a, GenOpFun, Availability)] -> DatatypeLibrary a
mkDtLib l = Prelude.foldl core Map.empty l
  where
    core dtlib (op,fun,av) = Map.insert (getObjType op, op) (fun, av) dtlib
