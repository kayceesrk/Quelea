{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell  #-}

module Codeec.Shim (
 runShimNode,
 mkDtLib
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

runShimNode :: OperationClass a
            => DatatypeLibrary a -> Backend -> Int -> IO ()
runShimNode dtlib backend port = do
  ctxt <- context
  sock <- socket ctxt Rep
  let myaddr = "tcp://*:" ++ show port
  bind sock myaddr
  serverJoin backend $ "tcp://localhost:" ++ show port
  forever $ do
    req <- receive sock
    result <- performOp dtlib $ decodeRequest req
    send sock [] result
  where
    performOp dtLib (Request objType operName arg) =
      let (op,_) = fromJust $ dtLib ^.at (objType, operName)
          (res, _) = op [] arg
      in return res

mkDtLib :: OperationClass a => [(a, GenOpFun, Availability)] -> DatatypeLibrary a
mkDtLib l = Prelude.foldl core Map.empty l
  where
    core dtlib (op,fun,av) = Map.insert (getObjType op, op) (fun, av) dtlib
