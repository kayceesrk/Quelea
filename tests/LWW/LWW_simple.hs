{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, CPP #-}

import Codeec.Shim
import Codeec.ClientMonad
import Codeec.DBDriver
import Codeec.Contract
import Codeec.NameService.Types
#ifdef LBB
  import Codeec.NameService.LoadBalancingBroker
#elif
  import Codeec.NameService.SimpleBroker
#endif
import Codeec.Marshall
import Codeec.TH

import System.Process (runCommand, terminateProcess)
import System.Environment (getExecutablePath, getArgs)
import Control.Concurrent (threadDelay)
import Database.Cassandra.CQL
import Control.Monad.Trans (liftIO)
import Data.Text (pack)
import Codeec.Types (summarize)
import Control.Monad (replicateM_, when, forever)
import Data.IORef

import LWWRegisterDefs

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559

data Kind = B | C | S | D | Drop | Create deriving (Read, Show)

keyspace :: Keyspace
keyspace = Keyspace $ pack "Codeec"

-------------------------------------------------------------------------------

tableName :: String
tableName = __TODO__

dtLib = mkDtLib [(HAWrite, mkGenOp writeReg summarize, $(checkOp HAWrite haWriteCtrt)),
                 (CAUWrite, mkGenOp writeReg summarize, $(checkOp CAUWrite haWriteCtrt)),
                 (STWrite, mkGenOp writeReg summarize, $(checkOp STWrite haWriteCtrt)),
                 (HARead, mkGenOp readReg summarize, $(checkOp HARead haReadCtrt)),
                 (CAURead, mkGenOp readReg summarize, $(checkOp CAURead haReadCtrt)),
                 (STRead, mkGenOp readReg summarize, $(checkOp STRead haReadCtrt))]

ecRead :: Key -> CSN Int
ecRead k = invoke k HARead ()

ccRead :: Key -> CSN Int
ccRead k = invoke k CAURead ()

scRead :: Key -> CSN Int
scRead k = invoke k STRead ()

ecWrite :: Key -> Int -> CSN ()
ecWrite k v = invoke k HAWrite v

ccWrite :: Key -> Int -> CSN ()
ccWrite k v = invoke k CAUWrite v

scWrite :: Key -> Int -> CSN ()
scWrite k v = invoke k STWrite v

{- daemon terminates after the given microseconds -}
terminateAfter :: Int
terminateAfter = 20000000

-------------------------------------------------------------------------------

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
      key <-
    D -> do
      let rtsArg = case restArgs of
                     [] -> ""
                     r:_ -> r
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTable tableName
      progName <- getExecutablePath
      putStrLn "Driver : Starting broker"
      b <- runCommand $ progName ++ " +RTS " ++ rtsArg ++ " -RTS B " ++ broker
      putStrLn "Driver : Starting server"
      s <- runCommand $ progName ++ " +RTS " ++ rtsArg ++ " -RTS S " ++ broker
      putStrLn "Driver : Starting client"
      c <- runCommand $ progName ++ " +RTS " ++ rtsArg ++ " -RTS C " ++ broker
      threadDelay terminateAfter
      mapM_ terminateProcess [b,s,c]
      runCas pool $ dropTable tableName
    Create -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTable tableName
    Drop -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ dropTable tableName
