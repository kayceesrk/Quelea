{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DoAndIfThenElse  #-}

module Codeec.ShimLayer.GC (
  maybeGCCache
) where

import Codeec.Types
import Codeec.ShimLayer.Types
import Codeec.DBDriver
import Control.Lens

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Concurrent.MVar
import System.Random (randomIO)
import Database.Cassandra.CQL

makeLenses ''CacheManager

-- Minimum high water mark size for GC
#define LWM 64

gcDB :: CacheManager -> ObjType -> Key -> GenSumFun -> IO ()
gcDB cm ot k gc = do
  sid <- randomIO
  rows <- runCas (cm^.pool) $ cqlRead ot ALL k
  -- get causal cut (rows)
  -- remove GC markers (rows)
  -- filter effects from unfinished sessions (rows)
  releaseLock ot k sid $ cm^.pool


maybeGCCache :: CacheManager -> ObjType -> Key -> Int -> GenSumFun -> IO ()
maybeGCCache cm ot k curSize gc | curSize < LWM = return ()
                           | otherwise = do
  cache <- takeMVar $ cm^.cacheMVar
  let ctxt = case M.lookup (ot, k) cache of
               Nothing -> []
               Just s -> map (\(a,e) -> e) $ S.toList s
  let newCtxt = gc ctxt
  newUUID <- randomIO
  let (newCache,_) = foldl (\(s,i) e -> (S.insert (Addr newUUID i, e) s, i+1)) (S.empty, 1) newCtxt
  hwm <- takeMVar $ cm^.hwmMVar
  putMVar (cm^.hwmMVar) $ M.insert (ot, k) (length newCtxt * 2) hwm
  putMVar (cm^.cacheMVar) $ M.insert (ot, k) newCache cache
  putStrLn $ "maybeGCCache : finalSize=" ++ (show $ length newCtxt)

