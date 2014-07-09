{-# LANGUAGE ScopedTypeVariables #-}

import Codeec.NameService.SimpleBroker
import System.Environment
import System.Process
import Control.Monad
import Control.Concurrent (threadDelay)


data Kind = B | C | S | D deriving (Read, Show)

brokerFE :: Frontend
brokerFE = Frontend "tcp://*:5558"

brokerBE :: Backend
brokerBE = Backend "tcp://*:5559"

main :: IO ()
main = do
  (kindStr:name:_) <- getArgs
  let k :: Kind = read kindStr
  case k of
    C -> forever $ do
      s <- clientJoin $ Frontend "tcp://localhost:5558"
      putStrLn $ name ++ " : " ++ s
      threadDelay 1000000
    S -> do
      serverJoin (Backend "tcp://localhost:5559") name
      forever $ threadDelay 1000000
    B -> do
      startBroker brokerFE brokerBE
    D -> do
      progName <- getExecutablePath
      putStrLn "Driver : Starting broker"
      b <- runCommand $ progName ++ " B broker"
      putStrLn "Driver : Starting server1"
      s1 <- runCommand $ progName ++ " S this-is-server1"
      putStrLn "Driver : Starting server2"
      s2 <- runCommand $ progName ++ " S this-is-server2"
      putStrLn "Driver : Starting client1"
      c <- runCommand $ progName ++ " C client1"
      putStrLn "Driver : Starting client2"
      c <- runCommand $ progName ++ " C client2"
      threadDelay 5000000
      mapM_ terminateProcess [b,s1,s2,c]
