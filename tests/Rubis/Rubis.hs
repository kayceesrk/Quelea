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
import Control.Monad (replicateM_, foldM, when, forever, foldM_, replicateM)
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

import RubisDefs
import RubisTxns

#define DEBUG

debugPrint :: String -> CSN ()
#ifdef DEBUG
debugPrint s = liftIO $ do
  tid <- myThreadId
  putStrLn $ "[" ++ (show tid) ++ "] " ++ s
#else
debugPrint _ = return ()
#endif

--------------------------------------------------------------------------------

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559

numOpsPerRound :: Num a => a
numOpsPerRound = 1

printEvery :: Int
printEvery = 1000

maxDelta :: Int
maxDelta = 100

minItemPrice :: Int
minItemPrice = 1000

waitBetweenSuccessfulBids :: Int
waitBetweenSuccessfulBids = 100000 -- 100ms

auctionTime :: Int
auctionTime = 5 * 1000000 -- 5 seconds

numBuyers :: Int
numBuyers = 8

numItems :: Int
numItems = 10

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
     <> value "1")
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
keyspace = Keyspace $ pack "Codeec"

dtLib = mkDtLib [(StockItem, mkGenOp stockItem summarize, $(checkOp StockItem stockItemCtrt)),
                 (RemoveFromStock, mkGenOp removeFromStock summarize, $(checkOp RemoveFromStock removeFromStockCtrt)),
                 (UpdateMaxBid, mkGenOp updateMaxBid summarize, $(checkOp UpdateMaxBid updateMaxBidCtrt)),
                 (ShowItem, mkGenOp showItem summarize, $(checkOp ShowItem showItemCtrt)),

                 (GetBalance, mkGenOp getBalance summarize, $(checkOp GetBalance getBidsByItemCtrt)),
                 (DepositToWallet, mkGenOp depositToWallet summarize, $(checkOp DepositToWallet depositToWalletCtrt)),
                 (WithdrawFromWallet, mkGenOp withdrawFromWallet summarize, $(checkOp WithdrawFromWallet withdrawFromWalletCtrt)),

                 (AddBid, mkGenOp addBid summarize, $(checkOp AddBid addBidCtrt)),
                 (CancelBid, mkGenOp cancelBid summarize, $(checkOp CancelBid cancelBidCtrt)),
                 (GetBid, mkGenOp getBid summarize, $(checkOp GetBid getBidCtrt)),

                 (AddItemBid, mkGenOp addItemBid summarize, $(checkOp AddItemBid addItemBidCtrt)),
                 (RemoveItemBid, mkGenOp removeItemBid summarize, $(checkOp RemoveItemBid removeItemBidCtrt)),
                 (GetBidsByItem, mkGenOp getBidsByItem summarize, $(checkOp GetBidsByItem getBidsByItemCtrt)),

                 (AddWalletBid, mkGenOp addWalletBid summarize, $(checkOp AddWalletBid addWalletBidCtrt)),
                 (RemoveWalletBid, mkGenOp removeWalletBid summarize, $(checkOp RemoveWalletBid removeWalletBidCtrt)),
                 (GetBidsByWallet, mkGenOp getBidsByWallet summarize, $(checkOp GetBidsByWallet getBidsByWalletCtrt)),

                 (AddWalletItem, mkGenOp addWalletItem summarize, $(checkOp AddWalletItem addWalletItemCtrt)),
                 (GetItemsByWallet, mkGenOp getItemsByWallet summarize, $(checkOp GetItemsByWallet getItemsByWalletCtrt))]

run :: Args -> IO ()
run args = do
  let k = read $ kind args
  let broker = brokerAddr args
  someTime <- getCurrentTime
  let ns = mkNameService (Frontend $ "tcp://" ++ broker ++ ":" ++ show fePort)
                         (Backend  $ "tcp://" ++ broker ++ ":" ++ show bePort) "localhost" 5560
  case k of
    Broker -> startBroker (Frontend $ "tcp://*:" ++ show fePort)
                     (Backend $ "tcp://*:" ++ show bePort)
    Server -> do
      runShimNode dtLib [("localhost","9042")] keyspace ns
    Client -> do
      let threads = read $ numThreads args
      mv::(MVar NominalDiffTime)<- newEmptyMVar
      replicateM_ threads $ forkIO $ do
        mvarList <- replicateM numBuyers $ newEmptyMVar
        mapM_ (\mv -> liftIO $ forkIO $ runSession ns $ buyerCore mv) mvarList
        runSession ns $ do
          wid <- newWallet 0
          sellItem mvarList wid numItems
        putMVar mv 0
      foldM_ (\l _ -> takeMVar mv >>= \newL -> return $ l + newL) 0 [1..threads]
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
      putStrLn "Driver : Starting client"
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


data Message = NewItem ItemID | Terminate

tryWinItem :: WalletID
           -> ItemID
           -> Int      -- My current max bid
           -> Int      -- My max price willing to pay
           -> CSN Bool -- True = Won!
tryWinItem wid iid currentBid maxPrice = do
  delta <- liftIO $ randomIO
  let newBidAmt = currentBid + delta `mod` maxDelta
  if newBidAmt > maxPrice
  then waitTillAuctionEnd
  else do
    bidResult <- bidForItem wid iid newBidAmt
    case bidResult of
      ItemNotYetAvailable -> do
        liftIO $ threadDelay waitBetweenSuccessfulBids
        tryWinItem wid iid currentBid maxPrice
      ItemGone -> amIMaxBidder iid wid
      PriceLow -> tryWinItem wid iid newBidAmt maxPrice
      BidSuccess bid -> do
        debugPrint $ "Item = " ++ (show iid) ++ " Buyer = " ++ (show wid) ++ " amt = " ++ (show newBidAmt)
        bideTime newBidAmt
  where
    waitTillAuctionEnd = do
      liftIO $ threadDelay waitBetweenSuccessfulBids
      item <- getItem iid
      case item of
        ItemRemoved -> amIMaxBidder iid wid
        otherwise -> waitTillAuctionEnd
    bideTime newBidAmt = do
      liftIO $ threadDelay waitBetweenSuccessfulBids
      -- Woken up. Check if the auction is still running.
      res <- getItem iid
      iAmMaxBidder <- amIMaxBidder iid wid
      case res of
        ItemRemoved -> return $ iAmMaxBidder
        otherwise -> -- Auction in progress
          if iAmMaxBidder
          then bideTime newBidAmt
          else tryWinItem wid iid newBidAmt maxPrice

buyerCore :: MVar Message -> CSN ()
buyerCore sellerMVar = do
  wid <- newWallet 10000000 -- Start with 10 MILLION dollars!
  msg <- liftIO $ takeMVar sellerMVar
  case msg of
    NewItem iid -> do
      scale <- liftIO $ randomIO >>= \i -> return $ i `mod` 10
      res <- tryWinItem wid iid minItemPrice (scale * minItemPrice)
      let WalletID widInt = wid
      let ItemID iidInt = iid
      when res $ liftIO $ putStrLn $ "Buyer " ++ show widInt ++ " won item " ++ show iidInt
      buyerCore sellerMVar
    Terminate -> return ()

sellItem :: [MVar Message] -> WalletID -> Int -> CSN ()
sellItem buyerList wid 0 = do
  mapM_ (\m -> liftIO $ putMVar m Terminate) buyerList
sellItem buyerList wid numItems = do
  iidInt <- liftIO $ randomIO >>= \i -> return $ i `mod` 1000000
  let iid = ItemID $ iidInt
  let desc = show $ iid
  openAuction wid iid desc minItemPrice
  debugPrint $ "sellItem: auction opened: ItemID = " ++ (show iid)
  mapM_ (\m -> liftIO $ putMVar m $ NewItem iid) buyerList
  liftIO $ threadDelay auctionTime
  debugPrint $ "sellItem: woken up: ItemID = " ++ (show iid)
  maxBidAmt <- concludeAuction wid iid
  liftIO $ putStrLn $ "Sold: " ++ (show iid) ++ " price = " ++ (show maxBidAmt)
  sellItem buyerList wid $ numItems - 1

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Run the Rubis benchmark"
     <> header "Rubis Benchmark" )
