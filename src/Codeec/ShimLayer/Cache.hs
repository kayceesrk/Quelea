{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell, DataKinds, OverloadedStrings  #-}

module Codeec.ShimLayer.Cache (
  Cache,

  initCache,
  addEffectToCache,
  getContext,
  getHistory,
  batchUpdate
) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString hiding (map, pack, putStrLn)
import Control.Lens
import qualified Data.Map as M
import Codeec.Types
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.State
import Data.Map.Lens
import Control.Monad (forever)

type History = M.Map SessUUID SeqNo
type Effect = ByteString

data ReqMsgs = Dummy1
data ResMsgs = Dummy2

data Cache = Cache (MVar ReqMsgs) (MVar ResMsgs)

data CacheState = CacheState {
  _cache   :: M.Map (ObjType, Key) ByteString,
  _history :: M.Map (ObjType, Key, SessUUID) SeqNo
}

type CacheMonad a = StateT CacheState IO a

makeLenses ''CacheState

cacheCore :: Cache -> CacheMonad ()
cacheCore (Cache reqMVar resMVar) = forever $ undefined

initCache :: IO Cache
initCache = undefined

addEffectToCache :: Cache -> ObjType -> Key -> SessUUID -> SeqNo -> ByteString -> IO Bool
addEffectToCache = undefined

getContext :: Cache -> ObjType -> Key -> IO [ByteString]
getContext = undefined

getHistory :: Int
getHistory = undefined

batchUpdate :: Int
batchUpdate = undefined
