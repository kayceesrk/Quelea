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
import MicroBlog1Ctrts

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

type Username = String
type Password = String

addNewUser :: UserID -> Username -> Password -> CSN Bool
addNewUser uid uname pwd = atomically ($(checkTxn "addNewUserTxn" addNewUserTxnCtrt)) $ do
  r::Bool <- invoke (mkKey uname) AddUsername uid
  if not r
  then return False {- username has already been taken -}
  else do {- success -}
    r::() <- invoke (mkKey uid) AddUser (uname,pwd)
    return True

getPassword :: Username -> CSN (Maybe Password)
getPassword uname = atomically ($(checkTxn "getPasswordTxn" getPasswordTxnCtrt)) $ do
  mbUid::Maybe UserID <- invoke (mkKey uname) GetUserID ()
  case mbUid of
    Nothing -> return Nothing
    Just uid -> do
      Just (_::String, pwd) <- invoke (mkKey uid) GetUserInfo ()
      return $ Just pwd

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
