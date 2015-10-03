module Quelea.NameService.Types (
  Frontend(..),
  Backend(..),
  NameService(..)
) where

import System.ZMQ4

newtype Frontend = Frontend { unFE :: String}
newtype Backend  = Backend  { unBE :: String}

data NameService = NameService {
  getFrontend   :: Frontend,
  getClientJoin :: IO (String, Socket Req),
  getServerJoin :: IO ()
}
