{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls  #-}

module Codeec.Shim (
 runShimNode
) where

import Codeec.Types
import Codeec.NameService.SimpleBroker
import Codeec.Marshall
import Database.Cassandra.CQL
import Data.Serialize
import Control.Applicative ((<$>))
import Control.Monad (forever)
import Data.ByteString hiding (map)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import System.ZMQ4
import Data.Maybe (fromJust)
import Control.Lens

performOp :: DatatypeLibrary -> Request -> IO ByteString
performOp dtLib (Request objType operName arg) =
  let im = fromJust $ dtLib ^.at objType
      (op,_) = fromJust $ im ^.at operName
      (res, _) = op [] arg
  in return res

runShimNode :: DatatypeLibrary -> Backend -> String -> Int -> IO ()
runShimNode dtlib backend serverName port = do
  ctxt <- context
  sock <- socket ctxt Rep
  let myaddr = "tcp://localhost:" ++ show port
  bind sock myaddr
  serverJoin backend myaddr
  forever $ do
    req <- receive sock
    result <- performOp dtlib $ decodeRequest req
    send sock [] result
