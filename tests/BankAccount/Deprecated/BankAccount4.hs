{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Quelea.Shim
import Quelea.ClientMonad
import Quelea.DBDriver
import BankAccountDefs
import Quelea.Contract
import System.Process (runCommand, terminateProcess)
import System.Environment (getExecutablePath, getArgs)
import Control.Concurrent (threadDelay)
import Quelea.NameService.SimpleBroker
import Quelea.Marshall
import Quelea.TH
import Database.Cassandra.CQL
import Control.Monad.Trans (liftIO)
import Data.Text (pack)
import Quelea.Client (mkKey)
import Control.Applicative
import Control.Monad (forever, when)
import Data.UUID
import System.Random (randomIO)

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


data Kind = B | C | S | D deriving (Read, Show)

keyspace :: Keyspace
keyspace = Keyspace $ pack "Quelea"

dtLib = mkDtLib [(Deposit, mkGenOp deposit summarize, $(checkOp Deposit depositCtrt)),
                 (Withdraw, mkGenOp withdraw summarize, $(checkOp Withdraw withdrawCtrt)),
                 (GetBalance, mkGenOp getBalance summarize, $(checkOp GetBalance getBalanceCtrt))]

main :: IO ()
main = do
  (kindStr:tailStr) <- getArgs
  let k :: Kind = read kindStr
  case k of
    B -> do
      let offStr:_ = tailStr
      let scale::Int = read offStr
      startBroker (Frontend $ "tcp://*:" ++ (show $ fePort*scale))
                  (Backend $ "tcp://*:" ++ (show $ bePort*scale))
    S -> do
      let offStr:_ = tailStr
      let off::Int = read offStr
      runShimNode dtLib [("localhost","9042")] keyspace
        (Backend $ "tcp://localhost:" ++ show (bePort*off)) (5560+off)
    C -> do
      let offStr:key1Str:key2Str:_ = tailStr
      let off::Int = read offStr
      let key1 = mkKey . readUUID $ key1Str
      let key2 = mkKey . readUUID $ key2Str
      runSession (Frontend $ "tcp://localhost:" ++ (show $ fePort * off)) $ do
        s <- getServerAddr
        liftIO $ putStrLn $ "Client " ++ show off ++ " : Connected to server " ++ s

        when (off /= 1) $ do
          liftIO $ putStrLn $ "Client " ++ show off ++ " : performing getBalance on key 1"
          r::Int <- invoke key1 GetBalance ()
          liftIO . putStrLn $ show r

        liftIO $ threadDelay 2000000
        atomically ReadCommitted $ do
          liftIO . putStrLn $ "Client " ++ show off ++ " : performing deposit on key 1"
          r::() <- invoke key1 Deposit (3+off)

          liftIO $ putStrLn $ "Client " ++ show off ++ " : performing deposit on key 2"
          r::() <- invoke key2 Deposit (5+off)

          when (off /= 1) $ do
            liftIO $ putStrLn $ "Client " ++ show off ++ " : performing getBalance in Txn on key 1"
            r::Int <- invoke key1 GetBalance ()
            liftIO . putStrLn $ show r

            liftIO $ putStrLn $ "Client " ++ show off ++ " : performing getBalance in Txn on key 2"
            r::Int <- invoke key2 GetBalance ()
            liftIO . putStrLn $ show r

        when (off /= 1) $ do
          liftIO $ putStrLn "Delaying thread for 2s"
          liftIO $ threadDelay 2000000

          liftIO $ putStrLn $ "Client " ++ show off ++ " : performing getBalance outside Txn on key 1"
          r::Int <- invoke key1 GetBalance ()
          liftIO . putStrLn $ show r

          liftIO $ putStrLn $ "Client " ++ show off ++ " : performing getBalance outside Txn on key 2"
          r::Int <- invoke key2 GetBalance ()
          liftIO . putStrLn $ show r

    D -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTxnTable >> createTable "BankAccount"
      progName <- getExecutablePath
      putStrLn "Driver : Starting broker0"
      b1 <- runCommand $ progName ++ " B 1"
      putStrLn "Driver : Starting broker1"
      b2 <- runCommand $ progName ++ " B 2"
      putStrLn "Driver : Starting server0"
      s0 <- runCommand $ progName ++ " S 1"
      putStrLn "Driver : Starting server1"
      s1 <- runCommand $ progName ++ " S 2"
      key1::UUID <- liftIO $ randomIO
      key2::UUID <- liftIO $ randomIO
      putStrLn "Driver : Starting client 1"
      c0 <- runCommand $ progName ++ " C 1 " ++ show key1 ++ " " ++ show key2
      threadDelay 3000000
      putStrLn "Driver : Starting client 2"
      c1 <- runCommand $ progName ++ " C 2 " ++ show key1 ++ " " ++ show key2
      threadDelay 6000000
      mapM_ terminateProcess [b1,b2,s0,s1,c0,c1]
      runCas pool $ dropTxnTable >> dropTable "BankAccount"
   where
     readUUID :: String -> UUID
     readUUID = read
