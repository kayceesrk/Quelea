{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Codeec.Shim
import Codeec.Client
import BankAccountDefs
import Codeec.Contract
import System.Process (runCommand, terminateProcess)
import System.Environment (getExecutablePath, getArgs)
import Control.Concurrent (threadDelay)
import Codeec.NameService.SimpleBroker
import Codeec.Marshall
import Codeec.TH

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


data Kind = B | C | S | D deriving (Read, Show)

dtLib = mkDtLib [(Deposit, mkGen deposit, $(check "Deposit" depositCtrt)),
                 (Withdraw, mkGen withdraw, $(check "Withdraw" withdrawCtrt)),
                 (GetBalance, mkGen getBalance, $(check "Withdraw" getBalanceCtrt))]

main :: IO ()
main = do
  (kindStr:_) <- getArgs
  let k :: Kind = read kindStr
  case k of
    B -> startBroker (Frontend $ "tcp://*:" ++ show fePort)
                     (Backend $ "tcp://*:" ++ show bePort)
    S -> do
      print dtLib
      runShimNode dtLib (Backend $ "tcp://localhost:" ++ show bePort) 5560
    C -> do
      sess <- beginSession $ Frontend $ "tcp://localhost:" ++ show fePort

      putStrLn "Client : performing deposit"
      r::() <- invoke sess Deposit (100::Int)

      putStrLn "Client : performing withdraw"
      r::(Maybe Int) <- invoke sess Withdraw (10::Int)
      putStrLn $ show r

      putStrLn "Client : performing getBalance"
      r::Int <- invoke sess GetBalance ()
      putStrLn $ show r
      endSession sess
    D -> do
      progName <- getExecutablePath
      putStrLn "Driver : Starting broker"
      b <- runCommand $ progName ++ " B"
      putStrLn "Driver : Starting server"
      s <- runCommand $ progName ++ " S"
      putStrLn "Driver : Starting client"
      c <- runCommand $ progName ++ " C"
      threadDelay 5000000
      mapM_ terminateProcess [b,s,c]

