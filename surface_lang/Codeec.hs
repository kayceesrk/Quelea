{-# LANGUAGE TemplateHaskell, OverloadedStrings, DataKinds, TypeSynonymInstances,
             FlexibleInstances, BangPatterns, EmptyDataDecls, ScopedTypeVariables  #-}

module Codeec (
-- Types
---------

EC,
Proc,
Effect(..),
Ctxt,
Key,
Table,

createTable,
performOp,
runEC,
effect,
performIO

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
import Control.Monad.IO.Class

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

type Proc a b = StateT ProcState CQL.Cas b

instance CQL.CasType Link where
  putCas (Link x y) = do
    putLazyByteString $ toByteString x
    (putWord64be . fromIntegral) y
  getCas = do
    x <- fromJust . fromByteString <$> getLazyByteString 16
    y <- fromIntegral <$> getWord64be
    return $ Link x y
  casType _ = CQL.CBlob

class CQL.CasType a => Effect a where

type Ctxt a = Gr a ()

mkCreateTable :: Table -> CQL.Query CQL.Schema () ()
mkCreateTable tname = CQL.query $ pack $ "create table " ++ tname ++ " (key uuid, sess uuid, at bigint, vis set<blob>, value int, primary key (key, sess, at)) "

mkInsert :: Table -> CQL.Query CQL.Write (Key, Sess, Int64, S.Set Link, a) ()
mkInsert tname = CQL.query $ pack $ "insert into " ++ tname ++ " (key, sess, at, vis, value) values (?, ?, ?, ?, ?)"

mkRead :: Table -> CQL.Query CQL.Rows (Key) (Key, Sess, Int64, S.Set Link, a)
mkRead tname = CQL.query $ pack $ "select key, sess, at, vis, value from " ++ tname ++ " where key=?"

createTable :: Table -> EC ()
createTable tname = liftIO . print =<< CQL.executeSchema CQL.ALL (mkCreateTable tname) ()

performIO :: IO a -> Proc b a
performIO = liftIO

effect :: (Effect a) => a -> Proc a ()
effect value = do
  k <- use key
  v <- use vis
  s <- use sess
  a <- use actid
  t <- use table
  actid += 1
  liftIO . print =<< CQL.executeWrite CQL.ONE (mkInsert t) (k, s, a + 1, v, value)

performOp :: (Effect a, Show res)
          => (Ctxt a -> arg -> Proc a res)
          -> Table
          -> Key
          -> arg
          -> EC res
performOp core tname k args = do
  rows <- CQL.executeRows CQL.ONE (mkRead tname) k
  liftIO $ putStrLn "1"
  !nodes <- zipWithM foo rows [0..(length rows)]
  liftIO $ putStrLn $ "2  " ++ (show $ length rows)
  let !ctxt = mkGraph nodes []
  liftIO $ putStrLn "3"
  sess <- liftIO randomIO
  liftIO $ putStrLn "4"
  let !ps = ProcState tname k (S.fromList [Link sess 0]) sess (0::Int64)
  liftIO $ putStrLn "5"
  res <- evalStateT (core ctxt args) ps
  liftIO $ putStrLn $ "HERE" ++ show res
  return res
  where
    foo (_, _, _, _, value) (i :: Node) = return (i, value)

runEC :: CQL.Pool -> EC a -> IO a
runEC = CQL.runCas
