{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell, DataKinds, OverloadedStrings  #-}

module Codeec.ShimLayer.UpdateFetcher (
  UpdateFetcher,

  initUpdateFetcher,
  addObject
) where

import Codeec.Types
import Data.Time.Clock.POSIX
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.Trans.State
import Control.Monad.Trans (liftIO)
import Data.Int (Int64)
import qualified Data.Map as M
import Control.Applicative

newtype POSIXTimeSec = POSIXTimeSec Int64 deriving (Eq, Ord, Show)

data UpdateFetcher = UpdateFetcher (MVar ()) (MVar ())

type EventTree = M.Map POSIXTimeSec (M.Map (ObjType, Key) Int64)

cFIRST_STEP :: Int64
cFIRST_STEP = 2


addTime :: Int64 -> POSIXTimeSec -> POSIXTimeSec
addTime delta (POSIXTimeSec t) = POSIXTimeSec (t + delta)

getCurrentPOSIXTimeSec :: IO POSIXTimeSec
getCurrentPOSIXTimeSec = (POSIXTimeSec . round) <$> getPOSIXTime

initUpdateFetcher :: IO UpdateFetcher
initUpdateFetcher = undefined

addObject :: UpdateFetcher -> ObjType -> Key -> IO ()
addObject = undefined
