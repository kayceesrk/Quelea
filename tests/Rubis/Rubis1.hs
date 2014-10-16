{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Codeec.Shim
import Codeec.ClientMonad
import Codeec.Marshall
import Codeec.NameService.SimpleBroker
import Codeec.TH

import System.Process (runCommand, terminateProcess)
import System.Environment (getExecutablePath, getArgs)
import Database.Cassandra.CQL
import Control.Monad.Trans (liftIO)
import Data.Text (pack)
import Codeec.Types (summarize)
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)

import RubisDefs
import RubisCtrts
import RubisTxns

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


data Kind = B | C | S | D | Drop deriving (Read, Show)

keyspace :: Keyspace
keyspace = Keyspace $ pack "Rubis"

dtLib = mkDtLib [(StockItem, mkGenOp stockItem summarize, $(checkOp StockItem stockItemCtrt)),
                 (RemoveFromStock, mkGenOp removeFromStock summarize, $(checkOp RemoveFromStock removeFromStockCtrt)),
                 (UpdateMaxBid, mkGenOp updateMaxBid summarize, $(checkOp UpdateMaxBid updateMaxBidCtrt)),
                 (ShowItem, mkGenOp showItem summarize, $(checkOp ShowItem showItemCtrt))]

main :: IO ()
main = do
  (kindStr:_) <- getArgs
  let k :: Kind = read kindStr
  case k of
    B -> startBroker (Frontend $ "tcp://*:" ++ show fePort)
                     (Backend $ "tcp://*:" ++ show bePort)

    S -> do
      runShimNode dtLib [("localhost","9042")] keyspace
        (Backend $ "tcp://localhost:" ++ show bePort) 5560

    C -> runSession (Frontend $ "tcp://localhost:" ++ show fePort) $ do
      key <- liftIO $ newKey
      r::() <- invoke key StockItem ("Organic Milk",2::Int, 20::Int)
      return ()

    D -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTables
      progName <- getExecutablePath
      putStrLn "Driver : Starting broker"
      b <- runCommand $ progName ++ " B"
      putStrLn "Driver : Starting server"
      s <- runCommand $ progName ++ " S"
      putStrLn "Driver : Starting client"
      c <- runCommand $ progName ++ " C"
      return ()
      {-threadDelay 5000000
      mapM_ terminateProcess [b,s,c]
      runCas pool $ dropTables-}

    Drop -> do
      pool <- newPool [("localhost", "9042")] keyspace Nothing
      runCas pool $ dropTables
