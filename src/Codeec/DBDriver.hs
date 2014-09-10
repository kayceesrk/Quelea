{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell, DataKinds, OverloadedStrings  #-}

module Codeec.DBDriver (
  TableName(..),
  RowValue(..),
  cqlRead,
  cqlWrite,
  createTable,
  dropTable
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
import Control.Lens
import Database.Cassandra.CQL
import Data.UUID
import Data.Int (Int64)
import qualified Data.Set as S
import Data.Text hiding (map)
import Control.Monad.Trans (liftIO)

--------------------------------------------------------------------------------
-- Cassandra Link Layer
--------------------------------------------------------------------------------

type TableName = String
type RowValue = (UUID, SessUUID, SeqNo, S.Set Addr, ByteString)
type Row = (SessUUID, SeqNo, S.Set Addr, ByteString)

mkCreateTable :: TableName -> Query Schema () ()
mkCreateTable tname = query $ pack $ "create table " ++ tname ++ " (objid uuid, sessid uuid, seqno bigint, deps set<blob>, value ascii, primary key (objid, sessid, seqno)) "

mkDropTable :: TableName -> Query Schema () ()
mkDropTable tname = query $ pack $ "drop table " ++ tname

mkInsert :: TableName -> Query Write RowValue ()
mkInsert tname = query $ pack $ "insert into " ++ tname ++ " (objid, sessid, seqno, deps, value) values (?, ?, ?, ?, ?)"

mkRead :: TableName -> Query Rows (UUID) Row
mkRead tname = query $ pack $ "select sessid, seqno, deps, value from " ++ tname ++ " where objid = ?"

cqlRead :: TableName -> Key -> Cas [Row]
cqlRead tname (Key k) = executeRows ONE (mkRead tname) k

cqlWrite :: TableName -> Key -> Row -> Cas ()
cqlWrite tname (Key k) (sid,sqn,dep,val) = executeWrite ONE (mkInsert tname) (k,sid,sqn,dep,val)

createTable :: TableName -> Cas ()
createTable tname = liftIO . print =<< executeSchema ALL (mkCreateTable tname) ()

dropTable :: TableName -> Cas ()
dropTable tname = liftIO . print =<< executeSchema ALL (mkDropTable tname) ()

