{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, CPP #-}

import Quelea.Shim
import Quelea.ClientMonad
import Quelea.DBDriver
import Quelea.Contract
import Control.Concurrent (ThreadId, myThreadId, forkIO, threadDelay, killThread)
import Quelea.NameService.Types
import Quelea.Types (summarize)
import Quelea.Marshall
import Quelea.TH
#ifdef LBB
import Quelea.NameService.LoadBalancingBroker
#else
import Quelea.NameService.SimpleBroker
#endif

import Language.Haskell.TH 
import Language.Haskell.TH.Syntax (Exp (..))
import Prelude hiding (catch)
import Control.Monad (replicateM_, foldM, when, forever)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException(..), AsyncException(..) , 
                          catch, handle, throw, assert, AssertionFailed(..))
import Data.IORef
import Data.Text (pack)
import Database.Cassandra.CQL
import Options.Applicative
import System.Environment (getExecutablePath, getArgs)
import System.Exit (exitSuccess)
import System.Posix.Signals
import System.Process (ProcessHandle, runCommand, terminateProcess)
import System.Random
import System.IO (hFlush, stdout)

import CounterDefs

--------------------------------------------------------------------------------

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559

tableName :: String
tableName = "Counter"

--------------------------------------------------------------------------------

data Kind = Broker | Client | Server
          | Daemon | Drop | Create deriving (Read, Show)

data Args = Args {
  -- Kind of process
  kind :: String,
  {- Daemon Options -}
  {- -------------- -}
  terminateAfter :: String,
  {- Client Options -}
  {- -------------- -}
  -- Number of client rounds
  numRounds :: String,
  -- Number of concurrent client threads
  numThreads :: String
}

args :: Parser Args
args = Args
  <$> strOption
      ( long "kind"
     <> metavar "[Broker|Client|Server|Daemon|Drop|Create]"
     <> help "Kind of process" )
  <*> strOption
      ( long "terminateAfter"
    <> metavar "SECS"
    <> help "Terminate child proceeses after time. Only relevant for Daemon"
    <> value "10")
  <*> strOption
      ( long "numRounds"
     <> metavar "NUM_ROUNDS"
     <> help "Number of client rounds"
     <> value "1000")
  <*> strOption
      ( long "numThreads"
     <> metavar "NUM_THREADS"
     <> help "Number of concurrent client threads"
     <> value "1")
-------------------------------------------------------------------------------

keyspace :: Keyspace
keyspace = Keyspace $ pack "Quelea"

[
  incCtrtA,
  readCtrtA ] = 
    $(do
        a <- checkOp Inc incCtrt
        b <- checkOp Read readCtrt
        le <- return $ (ListE::[Exp] -> Exp) [a,b]
        return le)

dtLib = do
  return $ mkDtLib [(Inc, mkGenOp incCount summarize, incCtrtA),
                    (Read, mkGenOp readCount summarize, readCtrtA)]

doRead :: Key -> CSN Int
doRead k = invoke k Read ()

doInc :: Key -> CSN ()
doInc k = do
  invoke k Inc ()

-------------------------------------------------------------------------------

run :: Args -> IO ()
run args = do
  let k = read $ kind args
  let broker = "localhost"
  let ns = mkNameService (Frontend $ "tcp://" ++ broker ++ ":" ++ show fePort)
                         (Backend  $ "tcp://" ++ broker ++ ":" ++ show bePort) "localhost" 5560
  case k of
    Broker -> startBroker (Frontend $ "tcp://*:" ++ show fePort)
                     (Backend $ "tcp://*:" ++ show bePort)
    Server -> do
      dtLib <- dtLib
      runShimNode dtLib [("localhost","9042")] keyspace ns
    Client -> do
      let rounds = read $ numRounds args
      let threads = read $ numThreads args

      key <- liftIO $ newKey

      replicateM_ threads $ forkIO $ do
        runSession ns $ foldM (clientCore args) 0 [1 .. rounds]
        return ()
    Create -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTable tableName
    Daemon -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTable tableName
      progName <- getExecutablePath
      putStrLn "Driver : Starting broker"
      b <- runCommand $ progName ++ " --kind Broker"
      putStrLn "Driver : Starting server"
      s <- runCommand $ progName ++ " --kind Server"
      putStrLn "Driver : Starting client"
      c <- runCommand $ progName ++ " --kind Client"
                        ++ " --numThreads " ++ (numThreads args)
                        ++ " --numRounds " ++ (numRounds args)
      -- Install handler for Ctrl-C
      tid <- myThreadId
      installHandler keyboardSignal (Catch $ reportSignal pool [b,s,c] tid) Nothing
      -- Block
      let termWait = read $ terminateAfter args
      threadDelay (termWait * 1000000)
      -- Woken up..
      mapM_ terminateProcess [b,s,c]
      runCas pool $ dropTable tableName
      putStrLn "Successfully verified monotonicity!"
    Drop -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ dropTable tableName

reportSignal :: Pool -> [ProcessHandle] -> ThreadId -> IO ()
reportSignal pool procList mainTid = do
  mapM_ terminateProcess procList
  runCas pool $ dropTable tableName
  killThread mainTid

doAssert :: Bool -> String -> CSN ()
doAssert b s = liftIO $ do
  return $ assert b ()
  `catch` (\(AssertionFailed _) -> putStrLn s)

clientCore :: Args -> Int -> Int -> CSN Int
clientCore args cnt round = do
  -- Generate key
  key <- liftIO $ (mkKey . (\i -> i `mod` (100000::Int))) <$> randomIO
  -- Perform the operations
  newCnt <- doRead key
  doAssert (newCnt >= cnt) "!!Error: Counter violated monotonicity!!"
  doInc key
  return newCnt

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "A simple monotonically increasing counter"
     <> header "Counter -- A simple monotonically increasing counter" )
