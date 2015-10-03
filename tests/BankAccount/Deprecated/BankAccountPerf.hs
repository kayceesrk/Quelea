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
import Quelea.Types (summarize)
import Control.Monad (replicateM_, forever, when)
import Data.IORef
import Control.Concurrent
import Data.Time.Clock
import System.IO

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


data Kind = B | C | S | D | Drop deriving (Read, Show)

keyspace :: Keyspace
keyspace = Keyspace $ pack "Quelea"

dtLib = mkDtLib [(Deposit, mkGenOp deposit summarize, $(checkOp Deposit depositCtrt)),
                 (Withdraw, mkGenOp withdraw summarize, $(checkOp Withdraw withdrawCtrt)),
                 (GetBalance, mkGenOp getBalance summarize, $(checkOp GetBalance getBalanceCtrt))]

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
      iter <- newIORef (0::Int, [])
      key <- newKey
      mv::(MVar Int)<- newEmptyMVar
      replicateM_ 8 $ forkIO $ runSession (Frontend $ "tcp://localhost:" ++ show fePort) $ do
        forever $ do
          t1 <- liftIO $ getCurrentTime
          r::() <- invoke key Deposit (1::Int)
          t2 <- liftIO $ getCurrentTime
          r::Int <- invoke key GetBalance ()
          (c,l::NominalDiffTime) <- liftIO $ atomicModifyIORef iter (\(c,l) ->
                     if c `mod` 100 == 0
                     then ((c+1,[diffUTCTime t2 t1]), (c, sum l / 100))
                     else ((c+1, (diffUTCTime t2 t1):l), (c, sum l / 100)))
          when (c `mod` 100 == 0) (liftIO . putStrLn $ "iter=" ++ show c ++ " count=" ++ show r ++ " latency=" ++ show l)
          liftIO $ hFlush stdout
        liftIO $ putMVar mv 0
      takeMVar mv
      return ()
    D -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTable "BankAccount"
      progName <- getExecutablePath
      putStrLn "Driver : Starting broker"
      b <- runCommand $ progName ++ " B"
      putStrLn "Driver : Starting server"
      s <- runCommand $ progName ++ " +RTS -N4 -RTS S"
      threadDelay 2000000
      putStrLn "Driver : Starting client"
      c <- runCommand $ progName ++ " +RTS -N4 -RTS C"
      threadDelay 60000000
      mapM_ terminateProcess [b,s,c]
      runCas pool $ dropTable "BankAccount"
    Drop -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ dropTable "BankAccount"
