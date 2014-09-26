{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell,
    DataKinds, OverloadedStrings, DoAndIfThenElse  #-}

module Codeec.DBDriver (
  TableName(..),
  RowValue(..),

  createTable,
  dropTable,
  cqlRead,
  cqlWrite,
  getLock,
  releaseLock
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
import Data.Maybe (fromJust)

-- Simply an alias for Types.ObjType
type TableName = String
type RowValue = (UUID, SessUUID, SeqNo, S.Set Addr, Cell)
type Row = (SessUUID, SeqNo, S.Set Addr, Cell)

--------------------------------------------------------------------------------
-- Cassandra Link Layer
--------------------------------------------------------------------------------

unlockedUUID :: SessUUID
unlockedUUID = fromJust $ fromString $ "123e4567-e89b-12d3-a456-426655440000"

mkCreateTable :: TableName -> Query Schema () ()
mkCreateTable tname = query $ pack $ "create table " ++ tname ++ " (objid uuid, sessid uuid, seqno bigint, deps set<blob>, value blob, primary key (objid, sessid, seqno)) "

mkDropTable :: TableName -> Query Schema () ()
mkDropTable tname = query $ pack $ "drop table " ++ tname

mkInsert :: TableName -> Query Write RowValue ()
mkInsert tname = query $ pack $ "insert into " ++ tname ++ " (objid, sessid, seqno, deps, value) values (?, ?, ?, ?, ?)"

mkRead :: TableName -> Query Rows (UUID) Row
mkRead tname = query $ pack $ "select sessid, seqno, deps, value from " ++ tname ++ " where objid = ? order by sessid, seqno"

-------------------------------------------------------------------------------

mkCreateLockTable :: TableName -> Query Schema () ()
mkCreateLockTable tname = query $ pack $ "create table " ++ tname ++ "_LOCK (objid uuid, sessid uuid, primary key (objid))"

mkDropLockTable :: TableName -> Query Schema () ()
mkDropLockTable tname = query $ pack $ "drop table " ++ tname ++ "_LOCK"

mkLockInsert :: TableName -> Query Write (UUID, SessUUID) ()
mkLockInsert tname = query $ pack $ "insert into " ++ tname ++ "_LOCK (objid, sessid) values (?, ?) if not exists"

mkLockUpdate :: TableName -> Query Write (SessUUID {- New -}, UUID, SessUUID {- Old -}) ()
mkLockUpdate tname = query $ pack $ "update " ++ tname ++ "_LOCK set sessid = ? where objid = ? if sessid = ?"

-------------------------------------------------------------------------------

cqlRead :: TableName -> Consistency -> Key -> Cas [Row]
cqlRead tname c (Key k) = executeRows c (mkRead tname) k

cqlWrite :: TableName -> Consistency -> Key -> Row -> Cas ()
cqlWrite tname c (Key k) (sid,sqn,dep,val) = do
  if S.size dep > 0
  then executeWrite c (mkInsert tname) (k,sid,sqn,dep,val)
  else executeWrite c (mkInsert tname) (k,sid,sqn, S.singleton $ Addr sid 0, val)

createTable :: TableName -> Cas ()
createTable tname = do
  liftIO . print =<< executeSchema ALL (mkCreateTable tname) ()
  liftIO . print =<< executeSchema ALL (mkCreateLockTable tname) ()

dropTable :: TableName -> Cas ()
dropTable tname = do
  liftIO . print =<< executeSchema ALL (mkDropTable tname) ()
  liftIO . print =<< executeSchema ALL (mkDropLockTable tname) ()

tryGetLock :: TableName -> Key -> SessUUID -> Bool {- tryInsert -} -> Cas Bool
tryGetLock tname (Key k) sid True = do
  res <- executeTrans (mkLockInsert tname) (k, sid)
  if res then return True
  else tryGetLock tname (Key k) sid False
tryGetLock tname (Key k) sid False = do
  res <- executeTrans (mkLockUpdate tname) (sid, k, unlockedUUID)
  if res then return True
  else tryGetLock tname (Key k) sid False

getLock :: TableName -> Key -> SessUUID -> Pool -> IO ()
getLock tname k sid pool = runCas pool $ do
  tryGetLock tname k sid True
  return ()

releaseLock :: TableName -> Key -> SessUUID -> Pool -> IO ()
releaseLock tname (Key k) sid pool = runCas pool $ do
  res <- executeTrans (mkLockUpdate tname) (unlockedUUID, k, sid)
  if res then return ()
  else error $ "releaseLock : key=" ++ show (Key k) ++ " sid=" ++ show sid
