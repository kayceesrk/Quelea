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
-- import System.Process (runCommand, terminateProcess)
import System.Posix.Process (forkProcess)
import System.Posix.Signals (signalProcess, killProcess, internalAbort)
import System.Process (ProcessHandle, runCommand, terminateProcess)
import System.Environment (getExecutablePath, getArgs)
import System.Environment (getExecutablePath, getArgs)
import Database.Cassandra.CQL
import Control.Monad.Trans (liftIO)
import Data.Text (pack)
import Quelea.Types (summarize)
import Control.Monad (replicateM_, void, ap, when, foldM)
import Control.Concurrent (threadDelay)
import Data.Time
import System.IO (hFlush, stdout)
import Data.Serialize
import Options.Applicative
import Control.Applicative
import System.Posix.Signals
import Control.Concurrent.MVar
import System.Random (randomIO)


import TPCCDefs
import TPCCCtrts
import TPCCTxns

[
  addYtdCtrtA,
  getandIncNextOIDCtrtA,
  addHistoryAmtCtrtA,
  addOrderCtrtA,
  addOrderlineCtrtA,
  addCustomerBalCtrtA,
  setCarrierCtrtA,
  setDeliveryDateCtrtA,
  getOlCntCtrtA,
  getYtdCtrtA,
  getHistoryAmtCtrtA,
  checkCarrierSetCtrtA,
  checkDeliverySetCtrtA] =
    $(do
        t1 <- runIO getCurrentTime
        a <- (checkOp AddYtd addYtdCtrt)
        c <- (checkOp GetAndIncNextOID getandIncNextOIDCtrt)
        d <- (checkOp AddHistoryAmt addHistoryAmtCtrt)
        f <- (checkOp AddOrder addOrderCtrt)
        h <- (checkOp AddOrderline addOrderlineCtrt)
        i <- (checkOp AddCustomerBal addCustomerBalCtrt)
        j <- (checkOp SetCarrier setCarrierCtrt)
        k <- (checkOp SetDeliveryDate setDeliveryDateCtrt)
        l <- (checkOp GetOlCnt getOlCntCtrt)
        m <- (checkOp GetYtd getYtdCtrt)
        n <- (checkOp GetHistoryAmt getHistoryAmtCtrt)
        o <- (checkOp CheckCarrierSet checkCarrierSetCtrt)
        p <- (checkOp CheckDeliverySet checkDeliverySetCtrt)
        le <- return $ (ListE::[Exp] -> Exp)
                [a, c, d, f, h, i, j, k, l, m, n, o, p]
        t2 <- runIO getCurrentTime
        _ <- runIO $ putStrLn $ "----------------------------------------------------------"
        _ <- runIO $ putStrLn $ "Classification of operation contracts completed in "++
                  (show $ diffUTCTime t2 t1)++"."
        _ <- runIO $ putStrLn $ "----------------------------------------------------------"
        _ <- runIO $ hFlush stdout
        return le)

dtLib = mkDtLib [(AddYtd, mkGenOp addYtd summarize, addYtdCtrtA),
                 (GetAndIncNextOID, mkGenOp getandIncNextOID summarize, getandIncNextOIDCtrtA),
                 (AddHistoryAmt, mkGenOp addHistoryAmt summarize, addHistoryAmtCtrtA),
                 (AddOrder, mkGenOp addOrder summarize, addOrderCtrtA),
                 (AddOrderline, mkGenOp addOrderline summarize, addOrderlineCtrtA),
                 (AddCustomerBal, mkGenOp addCustomerBal summarize, addCustomerBalCtrtA),
                 (SetCarrier, mkGenOp setCarrier summarize, setCarrierCtrtA),
                 (SetDeliveryDate, mkGenOp setDeliveryDate summarize, setDeliveryDateCtrtA),
                 (GetOlCnt, mkGenOp getOlCnt summarize, getOlCntCtrtA),
                 (GetYtd, mkGenOp getYtd summarize, getYtdCtrtA),
                 (GetHistoryAmt, mkGenOp getHistoryAmt summarize, getHistoryAmtCtrtA),
                 (CheckCarrierSet, mkGenOp checkCarrierSet summarize, checkCarrierSetCtrtA),
                 (CheckDeliverySet, mkGenOp checkDeliverySet summarize, checkDeliverySetCtrtA)]


--------------------------------------------------------------------------------

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559

numOpsPerRound :: Num a => a
numOpsPerRound = 4

printEvery :: Int
printEvery = 100

--------------------------------------------------------------------------------

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
  measureLatency :: Bool
}

(<>):: Monoid a => a -> a -> a
(<>) = mappend

args :: Parser Args
args = Args
  <$> strOption
      ( long "kind"
     <> metavar "KIND"
     <> help "Kind of process [Broker|Client|Server|Daemon|Drop|Create]" )
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
    <> value "600")
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
  <*> strOption
      ( long "delayReq"
     <> metavar "MICROSECS"
     <> help "Delay between client requests"
     <> value "0")
  <*> switch
      ( long "measureLatency"
     <> help "Measure operation latency" )
-------------------------------------------------------------------------------

keyspace :: Keyspace
keyspace = Keyspace $ pack "Quelea"

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
      runShimNodeWithOpts (read $ "GC_Full") 100000 0.5 dtLib [("localhost","9042")] keyspace ns
    Client -> do
      let rounds = read $ numRounds args
      let threads = read $ numThreads args

      mv::(MVar NominalDiffTime)<- newEmptyMVar

      t1 <- getCurrentTime
      replicateM_ threads $ forkIO $ do
        avgLatency <- runSession ns $ do
          liftIO $ putStrLn "Client running.."
          foldM (clientCore args delay someTime) 0 [1 .. rounds]
        putMVar mv avgLatency
      totalLat <- foldM (\l _ -> takeMVar mv >>= \newL -> return $ l + newL) 0 [1..threads]
      t2 <- getCurrentTime
      putStrLn $ "Throughput (ops/s) = " ++ (show $ (fromIntegral $ numOpsPerRound * rounds * threads) / (diffUTCTime t2 t1))
      putStrLn $ "Latency (s) = " ++ (show $ (totalLat / fromIntegral threads))
    Create -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTables
    Daemon -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTables
      progName <- getExecutablePath
      putStrLn "Driver : Starting broker"
      b <- runCommand $ progName ++ " +RTS " ++ (rtsArgs args)
                        ++ " -RTS --kind Broker --brokerAddr " ++ broker
      putStrLn "Driver : Starting server"
      s <- runCommand $ progName ++ " +RTS " ++ (rtsArgs args)
                        ++ " -RTS --kind Server --brokerAddr " ++ broker
      putStrLn "Driver : Starting client(1)"
      c <- runCommand $ progName ++ " +RTS " ++ (rtsArgs args)
                        ++ " -RTS --kind Client --brokerAddr " ++ broker
                        ++ " --numThreads " ++ (numThreads args)
                        ++ " --numRounds " ++ (numRounds args)
                        ++ " --delayReq " ++ (delayReq args)
                        ++ if (measureLatency args) then " --measureLatency" else ""
      putStrLn "Driver : Starting client(2)"
      c <- runCommand $ progName ++ " +RTS " ++ (rtsArgs args)
                        ++ " -RTS --kind Client --brokerAddr " ++ broker
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
      runCas pool $ dropTables
    Drop -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ dropTables

reportSignal :: Pool -> [ProcessHandle] -> ThreadId -> IO ()
reportSignal pool procList mainTid = do
  mapM_ terminateProcess procList
  runCas pool $ dropTables
  killThread mainTid

clientCore :: Args -> Int -> UTCTime -- default arguments
           -> NominalDiffTime -> Int -> CSN NominalDiffTime
clientCore args delay someTime avgLat round = do
  -- Generate info
  did <- DistrictID <$> mkInt 100
  wid <- WarehouseID <$> mkInt 1000
  cid <- CustomerID <$> mkInt 10000
  h_amt <- mkInt 10000
  ireq <- mkInt 10
  -- Delay thread if required
  when (delay /= 0) $ liftIO $ threadDelay delay
  -- Perform the operations
  t1 <- getNow args someTime
  liftIO $ putStrLn "doNewOrderTxn"
  oid <- doNewOrderTxn did wid ireq
  liftIO $ putStrLn "doDeliveryTxn"
  doDeliveryTxn wid did oid
  liftIO $ putStrLn "doPaymentTxn"
  doPaymentTxn h_amt wid did cid
  t2 <- getNow args someTime
  -- Calculate new latency
  let timeDiff = diffUTCTime t2 t1
  let newAvgLat = ((timeDiff / numOpsPerRound) + (avgLat * (fromIntegral $ round - 1))) / (fromIntegral round)
  return newAvgLat

mkInt :: Int -> CSN Int
mkInt max = liftIO $ (\i -> i `mod` max) <$> randomIO

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
     <> progDesc "Run the bank account benchmark"
     <> header "BankAccountBenchmark - A benchmark for bank account datatype on Quelea" )
