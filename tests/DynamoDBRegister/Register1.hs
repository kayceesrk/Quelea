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
import Data.Int (Int64)

import RegisterDefs

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


data Kind = B | C | S | D | Drop deriving (Read, Show)

keyspace :: Keyspace
keyspace = Keyspace $ pack "DynamoDBRegister"

dtLib = mkDtLib [(Put, mkGenOp putValue summarize, $(checkOp Put putValueCtrt)),
                 (Get, mkGenOp getValue summarize, $(checkOp Get getValueCtrt)),
                 (Inc, mkGenOp incValue summarize, $(checkOp Inc incValueCtrt)),
                 (Dec, mkGenOp decValue summarize, $(checkOp Dec decValueCtrt)),
                 (CondPut, mkGenOp condPutValue summarize, $(checkOp CondPut condPutValueCtrt)),
                 (StrongGet, mkGenOp getValue summarize, $(checkOp StrongGet strongGetValueCtrt))]

put :: Key -> Int64 -> CSN ()
put key v = invoke key Put v

get :: Key -> CSN Int64
get key = invoke key Get ()

condPut :: Key -> Int64 -> Int64 -> CSN Int64
condPut key old new = invoke key CondPut (old, new)

strongGet :: Key -> CSN Int64
strongGet key = invoke key StrongGet ()

inc :: Key -> Int64 -> CSN Int64
inc key v = invoke key Inc v

dec :: Key -> Int64 -> CSN Int64
dec key v = invoke key Dec v

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

    C -> do
      runSession (Frontend $ "tcp://localhost:" ++ show fePort) $ do
        key <- liftIO $ newKey
        put key 100
        inc key 10
        res <- get key
        liftIO . putStrLn $ "result " ++ show res
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
      threadDelay 5000000
      mapM_ terminateProcess [b,s,c]
      runCas pool $ dropTables

    Drop -> do
      pool <- newPool [("localhost", "9042")] keyspace Nothing
      runCas pool $ dropTables
