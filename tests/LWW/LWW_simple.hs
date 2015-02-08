{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, CPP #-}

import Codeec.Shim
import Codeec.ClientMonad
import Codeec.DBDriver
import Codeec.Contract
import Control.Concurrent (ThreadId, myThreadId, forkIO, threadDelay, killThread)
import Codeec.NameService.Types
import Codeec.Types (summarize)
import Codeec.Marshall
import Codeec.TH
#ifdef LBB
import Codeec.NameService.LoadBalancingBroker
#else
import Codeec.NameService.SimpleBroker
#endif

import Prelude hiding (catch)
import Control.Monad (replicateM_, foldM, when, forever)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar
import Control.Exception ( SomeException(..), AsyncException(..) , catch, handle, throw)
import Data.IORef
import Data.Text (pack)
import Data.Time
import Database.Cassandra.CQL
import Options.Applicative
import System.Environment (getExecutablePath, getArgs)
import System.Exit (exitSuccess)
import System.Posix.Signals
import System.Process (ProcessHandle, runCommand, terminateProcess)
import System.Random

import LWWRegisterDefs

--------------------------------------------------------------------------------

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559

tableName :: String
tableName = "LWWRegister"

numOpsPerRound :: Num a => a
numOpsPerRound = 2

printEvery :: Int
printEvery = 1000

--------------------------------------------------------------------------------

data Availability = Eventual | Strong | Causal deriving (Show, Read)

data Kind = Broker | Client | Server
          | Daemon | Drop | Create deriving (Read, Show)

data Args = Args {
  -- Kind of process
  kind :: String,
  -- Broker's address
  brokerAddr :: String,

  {- Daemon Options -}
  {- -------------- -}
  -- Optional rts arguments. Only relevant for Daemon.
  rtsArgs :: String,
  -- Terminate processes after time (microseconds). Only relevant for Daemon.
  terminateAfter :: String,

  {- Client Options -}
  {- -------------- -}
  -- Number of client rounds
  numRounds :: String,
  -- Number of concurrent client threads
  numThreads :: String,
  -- Delay between client requests in microseconds. Used to control throughput.
  delayReq :: String,
  -- Measure latency
  measureLatency :: Bool,
  -- Availability
  availability :: String
}

args :: Parser Args
args = Args
  <$> strOption
      ( long "kind"
     <> metavar "[Broker|Client|Server|Daemon|Drop|Create]"
     <> help "Kind of process" )
  <*> strOption
      ( long "brokerAddr"
     <> metavar "ADDR"
     <> help "Address of broker"
     <> value "localhost")
  <*> strOption
      ( long "rtsArgs"
     <> metavar "RTS_ARGS"
     <> help "RTS arguments passed to child processes. Only relevant for Daemon."
     <> value "")
  <*> strOption
      ( long "terminateAfter"
    <> metavar "SECS"
    <> help "Terminate child proceeses after time. Only relevant for Daemon"
    <> value "10")
  <*> strOption
      ( long "numRounds"
     <> metavar "NUM_ROUNDS"
     <> help "Number of client rounds"
     <> value "100000")
  <*> strOption
      ( long "numThreads"
     <> metavar "NUM_THREADS"
     <> help "Number of concurrent client threads"
     <> value "1")
  <*> strOption
      ( long "delayReq"
     <> metavar "MICROSECS"
     <> help "Delay between client requests"
     <> value "0")
  <*> switch
      ( long "measureLatency"
     <> help "Measure operation latency")
  <*> strOption
      ( long "availability"
     <> metavar "[Eventual|Causal|Strong]"
     <> help "Availability kind" )
-------------------------------------------------------------------------------

keyspace :: Keyspace
keyspace = Keyspace $ pack "Codeec"

dtLib = mkDtLib [(HAWrite, mkGenOp writeReg summarize, $(checkOp HAWrite haWriteCtrt)),
                 (CAUWrite, mkGenOp writeReg summarize, $(checkOp CAUWrite cauWriteCtrt)),
                 (STWrite, mkGenOp writeReg summarize, $(checkOp STWrite stWriteCtrt)),
                 (HARead, mkGenOp readReg summarize, $(checkOp HARead haReadCtrt)),
                 (CAURead, mkGenOp readReg summarize, $(checkOp CAURead cauReadCtrt)),
                 (STRead, mkGenOp readReg summarize, $(checkOp STRead stReadCtrt))]

ecRead :: Key -> CSN Int
ecRead k = invoke k HARead ()

ccRead :: Key -> CSN Int
ccRead k = invoke k CAURead ()

scRead :: Key -> CSN Int
scRead k = invoke k STRead ()

ecWrite :: Key -> Int -> CSN ()
ecWrite k v = do
  t <- liftIO $ getCurrentTime
  invoke k HAWrite (t,v)

ccWrite :: Key -> Int -> CSN ()
ccWrite k v = do
  t <- liftIO $ getCurrentTime
  invoke k CAUWrite (t,v)

scWrite :: Key -> Int -> CSN ()
scWrite k v = do
  t <- liftIO $ getCurrentTime
  invoke k STWrite (t,v)

-------------------------------------------------------------------------------

run :: Args -> IO ()
run args = do
  let k = read $ kind args
  let broker = brokerAddr args
  let delay = read $ delayReq args
  someTime <- getCurrentTime
  let ns = mkNameService (Frontend $ "tcp://" ++ broker ++ ":" ++ show fePort)
                         (Backend  $ "tcp://" ++ broker ++ ":" ++ show bePort) "localhost" 5560
  case k of
    Broker -> startBroker (Frontend $ "tcp://*:" ++ show fePort)
                     (Backend $ "tcp://*:" ++ show bePort)
    Server -> do
      runShimNode dtLib [("localhost","9042")] keyspace ns
    Client -> do
      let rounds = read $ numRounds args
      let threads = read $ numThreads args

      key <- liftIO $ newKey
      mv::(MVar NominalDiffTime)<- newEmptyMVar

      t1 <- getCurrentTime
      replicateM_ threads $ forkIO $ do
        avgLatency <- runSession ns $ foldM (clientCore args delay someTime) 0 [1 .. rounds]
        putMVar mv avgLatency
      totalLat <- foldM (\l _ -> takeMVar mv >>= \newL -> return $ l + newL) 0 [1..threads]
      t2 <- getCurrentTime
      putStrLn $ "Throughput (ops/s) = " ++ (show $ (fromIntegral $ numOpsPerRound * rounds * threads) / (diffUTCTime t2 t1))
      putStrLn $ "Latency (s) = " ++ (show $ (totalLat / fromIntegral threads))
    Create -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTable tableName
    Daemon -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTable tableName
      progName <- getExecutablePath
      putStrLn "Driver : Starting broker"
      b <- runCommand $ progName ++ " +RTS " ++ (rtsArgs args)
                        ++ " -RTS --kind Broker --brokerAddr " ++ broker
                        ++ " --availability " ++ (availability args)
      putStrLn "Driver : Starting server"
      s <- runCommand $ progName ++ " +RTS " ++ (rtsArgs args)
                        ++ " -RTS --kind Server --brokerAddr " ++ broker
                        ++ " --availability " ++ (availability args)
      putStrLn "Driver : Starting client"
      c <- runCommand $ progName ++ " +RTS " ++ (rtsArgs args)
                        ++ " -RTS --kind Client --brokerAddr " ++ broker
                        ++ " --availability " ++ (availability args)
                        ++ " --numThreads " ++ (numThreads args)
                        ++ " --numRounds " ++ (numRounds args)
                        ++ " --delayReq " ++ (delayReq args)
                        ++ if (measureLatency args) then " --measureLatency" else ""
      -- Install handler for Ctrl-C
      tid <- myThreadId
      installHandler keyboardSignal (Catch $ reportSignal pool [b,s,c] tid) Nothing
      -- Block
      let termWait = read $ terminateAfter args
      threadDelay (termWait * 1000000)
      -- Woken up..
      mapM_ terminateProcess [b,s,c]
      runCas pool $ dropTable tableName
    Drop -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ dropTable tableName

reportSignal :: Pool -> [ProcessHandle] -> ThreadId -> IO ()
reportSignal pool procList mainTid = do
  mapM_ terminateProcess procList
  runCas pool $ dropTable tableName
  killThread mainTid

clientCore :: Args -> Int -> UTCTime -- default arguments
           -> NominalDiffTime -> Int -> CSN NominalDiffTime
clientCore args delay someTime avgLat round = do
  -- Generate key
  key <- liftIO $ (mkKey . (\i -> i `mod` (100000::Int))) <$> randomIO
  -- Delay thread if required
  when (delay /= 0) $ liftIO $ threadDelay delay
  -- Perform the operations
  t1 <- getNow args someTime
  randInt <- liftIO $ randomIO
  case read $ availability args of
    Eventual -> ecRead key >> ecWrite key randInt
    Causal -> ccRead key >> ccWrite key randInt
    Strong -> scRead key >> scWrite key randInt
  t2 <- getNow args someTime
  -- Calculate new latency
  let timeDiff = diffUTCTime t2 t1
  let newAvgLat = ((timeDiff / numOpsPerRound) + (avgLat * (fromIntegral $ round - 1))) / (fromIntegral round)
  -- Print info if required
  when (round `mod` printEvery == 0) $ do
    liftIO . putStrLn $ "Round = " ++ show round
                        ++ if (measureLatency args) then " latency = " ++ show newAvgLat else ""
  return newAvgLat

getNow :: Args -> UTCTime -> CSN UTCTime
getNow args someTime =
  if (measureLatency args)
  then liftIO $ getCurrentTime
  else return someTime

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Run the LWW simple benchmark"
     <> header "LWW Simple -- A benchmark for testing the performance of least-write-wins register" )
