{-# LANGUAGE TemplateHaskell, OverloadedStrings, DataKinds, TypeSynonymInstances,
             FlexibleInstances, BangPatterns, EmptyDataDecls, ScopedTypeVariables  #-}

module Codeec (
-- Types
---------

EC,
Proc,

Effect,
Ctxt,
Key,

Table,

createTable,
performOp,

)
where

{-
 * TODO
 * ----
 * (1) A way to describe the store (tables) -- Essentially a datatype
 * definition whose values represent the write performed on the table.
 *
 * (2) A way to describe operations on the store (Stored procedures)
 * (2.1) The procedures are restricted to single-key.
 * (2.2) Can perform effects on the store.
 *
 * (3) A way to describe the proedure-level consistency.
 *
 * (4) A wat to describe global consistency.
 *
 * Tie together (3) and (4) with (1) and (2).
 -}

import Data.Text (Text, pack)
import Language.Haskell.TH
import qualified Database.Cassandra.CQL as CQL
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph
import SpecCheck
import Data.UUID
import Control.Lens hiding (Action)
import Control.Monad.Trans.State
import Data.Data
import qualified Data.Set as S
import Data.Maybe
import Data.Serialize
import Control.Applicative
import Data.Int (Int64)
import Control.Monad.Trans (liftIO)
import Control.Monad (zipWithM)
import System.Random

type EC a = CQL.Cas a
type Key  = UUID
type Sess = UUID
type Table = String
data Link = Link UUID Int64 deriving (Eq, Ord, Read, Show)

data ProcState = ProcState {
                  _table :: String,
                  _key   :: Key,
                  _vis   :: S.Set Link,
                  _sess  :: Sess,
                  _actid :: Int64
                }

makeLenses ''ProcState

data Proc a b = Proc { unProc :: StateT ProcState CQL.Cas b }

instance CQL.CasType Link where
  putCas (Link x y) = do
    putLazyByteString $ toByteString x
    (putWord64be . fromIntegral) y
  getCas = do
    x <- fromJust . fromByteString <$> getLazyByteString 16
    y <- fromIntegral <$> getWord64be
    return $ Link x y
  casType _ = CQL.CBlob

instance Monad (Proc a) where
  Proc r >>= f = Proc $ r >>= unProc . f
  return = return

class CQL.CasType a => Effect a
type Ctxt a = Gr a ()

mkCreateTable :: Table -> CQL.Query CQL.Schema () ()
mkCreateTable tname = CQL.query $ pack $ "create table " ++ tname ++ " (key uuid, sess uuid, at int, value blob, vis set<blob>)"

mkInsert :: Table -> CQL.Query CQL.Write (Key, Sess, Int64, a, S.Set Link) ()
mkInsert tname = CQL.query $ pack $ "insert into " ++ tname ++ " (key, sess, at, value, vis) values (?, ?, ?, ?, ?)"

mkRead :: Table -> CQL.Query CQL.Rows (Key) (Key, Sess, Int64, a, S.Set Link)
mkRead tname = CQL.query $ pack $ "select * from " ++ tname ++ " where key=?"

createTable :: Table -> EC ()
createTable tname = liftIO . print =<< CQL.executeSchema CQL.ALL (mkCreateTable tname) ()

effect :: (Effect a) => a -> Proc a ()
effect value = Proc $ do
  k <- use key
  v <- use vis
  s <- use sess
  a <- use actid
  t <- use table
  actid += 1
  CQL.executeWrite CQL.ONE (mkInsert t) (k, s, a + 1, value, v)

performOp :: Effect a
          => (Ctxt a -> arg -> Proc a res)
          -> Table
          -> Key
          -> arg
          -> EC res
performOp core tname k args = do
  rows <- CQL.executeRows CQL.ONE (mkRead tname) k
  nodes <- zipWithM foo rows [1..(length rows)]
  let ctxt = mkGraph nodes []
  sess <- liftIO randomIO
  let ps = ProcState tname k S.empty sess 1
  (res, _) <- runStateT (unProc $ core ctxt args) ps
  return res
  where
    foo (_, _, _, value, _) (i :: Node) = return (i, value)

