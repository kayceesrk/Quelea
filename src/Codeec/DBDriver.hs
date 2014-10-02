{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell,
    DataKinds, OverloadedStrings, DoAndIfThenElse  #-}

module Codeec.DBDriver (
  TableName(..),
  ReadRow,

  createTable,
  dropTable,
  cqlRead,
  cqlInsert,
  cqlDelete,
  getLock,
  releaseLock,

  createTxnTable,
  dropTxnTable,
  readTxn,
  insertTxn
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
type WriteRow = (UUID, UUID, SeqNo, S.Set Addr, Cell, Maybe UUID)
type ReadRowInternal = (UUID, SeqNo, S.Set Addr, Cell, Maybe UUID)
type ReadRow = (SessID, SeqNo, S.Set Addr, Cell, Maybe TxnID)

--------------------------------------------------------------------------------
-- Cassandra Link Layer
--------------------------------------------------------------------------------


-- A Row either corresponds to an effect (Cell is EffectVal bs) or a gc marker
-- (Cell is GCMarker). In case of GCMarker, the dependence set (deps value in
-- the row) is interpreted as a "Cursor". All effects that are encapsulated by
-- this cursor are considered to have been GC'ed.
mkCreateTable :: TableName -> Query Schema () ()
mkCreateTable tname = query $ pack $ "create table " ++ tname ++
                      " (objid uuid, sessid uuid, seqno bigint, deps set<blob>, value blob, txnid uuid, primary key (objid, sessid, seqno)) "

mkDropTable :: TableName -> Query Schema () ()
mkDropTable tname = query $ pack $ "drop table " ++ tname

mkInsert :: TableName -> Query Write WriteRow ()
mkInsert tname = query $ pack $ "insert into " ++ tname ++ " (objid, sessid, seqno, deps, value, txnid) values (?, ?, ?, ?, ?, ?)"

mkDelete :: TableName -> Query Write (UUID, UUID, SeqNo) ()
mkDelete tname = query $ pack $ "delete from " ++ tname ++ " where objid = ? and sessid = ? and seqno = ?"

mkRead :: TableName -> Query Rows (UUID) ReadRowInternal
mkRead tname = query $ pack $ "select sessid, seqno, deps, value, txnid from " ++ tname ++ " where objid = ? order by sessid, seqno"

-------------------------------------------------------------------------------

mkCreateLockTable :: TableName -> Query Schema () ()
mkCreateLockTable tname = query $ pack $ "create table " ++ tname ++ "_LOCK (objid uuid, sessid uuid, primary key (objid))"

mkDropLockTable :: TableName -> Query Schema () ()
mkDropLockTable tname = query $ pack $ "drop table " ++ tname ++ "_LOCK"

mkLockInsert :: TableName -> Query Write (UUID, UUID) ()
mkLockInsert tname = query $ pack $ "insert into " ++ tname ++ "_LOCK (objid, sessid) values (?, ?) if not exists"

mkLockUpdate :: TableName -> Query Write (UUID {- New -}, UUID, UUID {- Old -}) ()
mkLockUpdate tname = query $ pack $ "update " ++ tname ++ "_LOCK set sessid = ? where objid = ? if sessid = ?"

-------------------------------------------------------------------------------

mkCreateTxnTable :: Query Schema () ()
mkCreateTxnTable = "create table Txns (txnid uuid, deps set<blob>, primary key (txnid))"

mkDropTxnTable :: Query Schema () ()
mkDropTxnTable = "drop table Txns"

mkInsertTxnTable :: Query Write (UUID, S.Set TxnDep) ()
mkInsertTxnTable = "insert into Txns (txnid, deps) values (?, ?)"

mkReadTxnTable :: Query Rows (UUID) (S.Set TxnDep)
mkReadTxnTable = "select deps from Txns where txnid = ?"

-------------------------------------------------------------------------------

cqlRead :: TableName -> Consistency -> Key -> Cas [ReadRow]
cqlRead tname c (Key k) = do
  rows <- executeRows c (mkRead tname) k
  return $ map (\(sid, sqn, deps, val, txid) -> (SessID sid, sqn, deps, val, TxnID <$> txid)) rows


cqlInsert :: TableName -> Consistency -> Key -> ReadRow -> Cas ()
cqlInsert tname c (Key k) (SessID sid,sqn,dep,val,txid) = do
  if sqn == 0
  then error "cqlInsert : sqn is 0"
  else do
    if S.size dep > 0
    then executeWrite c (mkInsert tname) (k,sid,sqn,dep,val,unTxnID <$> txid)
    else executeWrite c (mkInsert tname) (k,sid,sqn, S.singleton $ Addr (SessID sid) 0, val, unTxnID <$> txid)

cqlDelete :: TableName -> Key -> SessID -> SeqNo -> Cas ()
cqlDelete tname (Key k) (SessID sid) sqn = executeWrite ALL (mkDelete tname) (k,sid,sqn)

createTxnTable :: Cas ()
createTxnTable = liftIO . print =<< executeSchema ALL mkCreateTxnTable ()

dropTxnTable :: Cas ()
dropTxnTable = liftIO . print =<< executeSchema ALL mkDropTxnTable ()

insertTxn :: TxnID -> S.Set TxnDep -> Cas ()
insertTxn (TxnID txnid) deps = do
  executeWrite ONE mkInsertTxnTable (txnid, deps)

readTxn :: TxnID -> Cas (Maybe (S.Set TxnDep))
readTxn (TxnID txnid) = executeRow ONE mkReadTxnTable txnid

createTable :: TableName -> Cas ()
createTable tname = do
  liftIO . print =<< executeSchema ALL (mkCreateTable tname) ()
  liftIO . print =<< executeSchema ALL (mkCreateLockTable tname) ()

dropTable :: TableName -> Cas ()
dropTable tname = do
  liftIO . print =<< executeSchema ALL (mkDropTable tname) ()
  liftIO . print =<< executeSchema ALL (mkDropLockTable tname) ()

tryGetLock :: TableName -> Key -> SessID -> Bool {- tryInsert -} -> Cas Bool
tryGetLock tname (Key k) (SessID sid) True = do
  res <- executeTrans (mkLockInsert tname) (k, sid)
  if res then return True
  else tryGetLock tname (Key k) (SessID sid) False
tryGetLock tname (Key k) (SessID sid) False = do
  res <- executeTrans (mkLockUpdate tname) (sid, k, knownUUID)
  if res then return True
  else tryGetLock tname (Key k) (SessID sid) False

getLock :: TableName -> Key -> SessID -> Pool -> IO ()
getLock tname k sid pool = runCas pool $ do
  tryGetLock tname k sid True
  return ()

releaseLock :: TableName -> Key -> SessID -> Pool -> IO ()
releaseLock tname (Key k) (SessID sid) pool = runCas pool $ do
  res <- executeTrans (mkLockUpdate tname) (knownUUID, k, sid)
  if res then return ()
  else error $ "releaseLock : key=" ++ show (Key k) ++ " sid=" ++ show sid
