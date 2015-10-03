{-# Language TemplateHaskell, EmptyDataDecls, ScopedTypeVariables,
    TypeSynonymInstances, FlexibleInstances #-}

module Quelea.ShimLayer.Types (
  CacheManager(..),
  CacheMap,
  NearestDeps,
  NearestDepsMap,
  CursorMap,
  CursorAtKey,
  Effect
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.ByteString
import Control.Concurrent.MVar
import Database.Cassandra.CQL
import Data.Time

import Quelea.Types

type Effect = ByteString

type CacheMap    = (M.Map (ObjType, Key) (S.Set (Addr, Effect)))
type HwmMap      = M.Map (ObjType, Key) Int
type DiskRowCount = M.Map (ObjType, Key) Int
type Cache       = MVar CacheMap
type CursorAtKey = M.Map SessID SeqNo
type CursorMap   = (M.Map (ObjType, Key) CursorAtKey)
type Cursor      = MVar CursorMap
type NearestDepsMap = (M.Map (ObjType, Key) (S.Set Addr))
type NearestDeps  = MVar NearestDepsMap
type HotLocs     = MVar (S.Set (ObjType, Key))
type Semaphore   = MVar ()
type ThreadQueue = MVar ([MVar ()])


data CacheManager = CacheManager {
  _cacheMVar        :: Cache,
  _cursorMVar       :: Cursor,
  _depsMVar         :: NearestDeps,
  _lastGCAddrMVar   :: MVar (M.Map (ObjType, Key) SessID),
  _lastGCTimeMVar   :: MVar (M.Map (ObjType, Key) UTCTime),
  _includedTxnsMVar :: MVar (S.Set TxnID, M.Map (ObjType,Key) (S.Set TxnID)),

  _hwmMVar          :: MVar HwmMap,
  _diskRowCntMVar   :: MVar DiskRowCount,
  _hotLocsMVar      :: HotLocs,
  _semMVar          :: Semaphore,
  _blockedMVar      :: ThreadQueue,
  _pool             :: Pool
}
