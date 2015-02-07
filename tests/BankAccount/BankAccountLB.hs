{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Codeec.Shim
import Codeec.ClientMonad
import Codeec.DBDriver
import BankAccountDefs
import Codeec.Contract
import System.Process (runCommand, terminateProcess)
import System.Environment (getExecutablePath, getArgs)
import Control.Concurrent (threadDelay)
import Codeec.NameService.Types
import Codeec.NameService.SimpleBroker
-- import Codeec.NameService.LoadBalancingBroker
import Codeec.Marshall
import Codeec.TH
import Database.Cassandra.CQL
import Control.Monad.Trans (liftIO)
import Data.Text (pack)
import Codeec.Types (summarize)
import Control.Monad (replicateM_, when, forever)
import Data.IORef

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


data Kind = B | C | S | D | Drop | Create deriving (Read, Show)

keyspace :: Keyspace
keyspace = Keyspace $ pack "Codeec"

dtLib = mkDtLib [(Deposit, mkGenOp deposit summarize, $(checkOp Deposit depositCtrt)),
                 (Withdraw, mkGenOp withdraw summarize, $(checkOp Withdraw withdrawCtrt)),
                 (GetBalance, mkGenOp getBalance summarize, $(checkOp GetBalance getBalanceCtrt))]

main :: IO ()
main = do
  (kindStr:broker:restArgs) <- getArgs
  let k :: Kind = read kindStr
  let ns = mkNameService (Frontend $ "tcp://" ++ broker ++ ":" ++ show fePort)
                         (Backend  $ "tcp://" ++ broker ++ ":" ++ show bePort) "localhost" 5560
  case k of
    B -> startBroker (Frontend $ "tcp://*:" ++ show fePort)
                     (Backend $ "tcp://*:" ++ show bePort)
    S -> do
      runShimNode dtLib [("localhost","9042")] keyspace ns
    C -> runSession ns $ do
      key <- liftIO $ newKey
      cnt <- liftIO $ newIORef 1
      replicateM_ 100000 $ do
        r::() <- invoke key Deposit (2::Int)
        -- r::() <- invoke key Withdraw (1::Int)
        r :: Int <- invoke key GetBalance ()
        round <- liftIO $ readIORef cnt
        when (round `mod` 1000 == 0) $ do
          liftIO . putStrLn $ "Round = " ++ show round ++ " result = " ++ show r
        liftIO $ writeIORef cnt $ round + 1
      forever $ do
        r :: Int <- invoke key GetBalance ()
        liftIO . putStrLn $ "Balance = " ++ show r
        liftIO $ threadDelay 200000
    Create -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTable "BankAccount"
    D -> do
      let rtsArg = case restArgs of
                     [] -> ""
                     r:_ -> r
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTable "BankAccount"
      progName <- getExecutablePath
      putStrLn "Driver : Starting broker"
      b <- runCommand $ progName ++ " +RTS " ++ rtsArg ++ " -RTS B " ++ broker
      putStrLn "Driver : Starting server"
      s <- runCommand $ progName ++ " +RTS " ++ rtsArg ++ " -RTS S " ++ broker
      putStrLn "Driver : Starting client"
      c <- runCommand $ progName ++ " +RTS " ++ rtsArg ++ " -RTS C " ++ broker
      threadDelay 20000000
      mapM_ terminateProcess [b,s,c]
      runCas pool $ dropTable "BankAccount"
    Drop -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ dropTable "BankAccount"
