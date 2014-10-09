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

import MicroBlogDefs
import MicroBlogTxns

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


data Kind = B | C | S | D | Drop deriving (Read, Show)

keyspace :: Keyspace
keyspace = Keyspace $ pack "MicroBlog"

dtLib = mkDtLib [(AddUser, mkGenOp addUser summarize, $(checkOp AddUser addUserCtrt)),
                 (AddUsername, mkGenOp addUsername summarize, $(checkOp AddUsername addUsernameCtrt)),
                 (GetUserID, mkGenOp getUserID summarize, $(checkOp GetUserID getUserIDCtrt)),
                 (GetUserInfo, mkGenOp getUserInfo summarize, $(checkOp GetUserInfo getUserInfoCtrt))]

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
      r::() <- invoke key AddUser ("Alice","test123")
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
