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
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO (hFlush, stdout)

import RubisDefs
import RubisTxns

-- #define DEBUG

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

auctionTime :: Int
auctionTime = 5 * 1000000 -- 5 seconds

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
  -- Number of concurrent client threads
  numAuctions :: String,
  -- Number of Buyers per auction
  numBuyers :: String,
  -- Delay between client requests in microseconds. Used to control throughput.
  delayReq :: String,
  -- Number of items per auction
  numItems :: String
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
      ( long "numAuctions"
     <> metavar "NUM_AUCTIONS"
     <> help "Number of concurrent auctions"
     <> value "1")
  <*> strOption
      ( long "numBuyers"
     <> metavar "NUM_BUYERS"
     <> help "Number of buyers per auction"
     <> value "3")
  <*> strOption
      ( long "delayReq"
     <> metavar "MICROSECS"
     <> help "Delay between client requests"
     <> value "100000")
  <*> strOption
      ( long "numItems"
     <> metavar "NUM_ITEMS"
     <> help "Number of items sold per auction"
     <> value "10")


-------------------------------------------------------------------------------

keyspace :: Keyspace
keyspace = Keyspace $ pack "Quelea"

[
  stockItemCtrtA,
  removeFromStockCtrtA,
  updateMaxBidCtrtA,
  showItemCtrtA,
  getBidsByItemCtrtA,
  depositToWalletCtrtA,
  withdrawFromWalletCtrtA,
  addBidCtrtA,
  cancelBidCtrtA,
  getBidCtrtA,
  addItemBidCtrtA,
  removeItemBidCtrtA,
  addWalletBidCtrtA,
  removeWalletBidCtrtA,
  getBidsByWalletCtrtA,
  addWalletItemCtrtA,
  getItemsByWalletCtrtA,
  getBalanceCtrtA ] =
    $(do
        t1 <- runIO getCurrentTime
        a1 <- checkOp StockItem stockItemCtrt
        a2 <- checkOp RemoveFromStock removeFromStockCtrt
        a3 <- checkOp UpdateMaxBid updateMaxBidCtrt
        a4 <- checkOp ShowItem showItemCtrt
        a5 <- checkOp GetBidsByItem getBidsByItemCtrt
        a6 <- checkOp DepositToWallet depositToWalletCtrt
        a7 <- checkOp WithdrawFromWallet withdrawFromWalletCtrt
        b1 <- checkOp AddBid addBidCtrt
        b2 <- checkOp CancelBid cancelBidCtrt
        b3 <- checkOp GetBid getBidCtrt
        b4 <- checkOp AddItemBid addItemBidCtrt
        b5 <- checkOp RemoveItemBid removeItemBidCtrt
        b7 <- checkOp AddWalletBid addWalletBidCtrt
        c1 <- checkOp RemoveWalletBid removeWalletBidCtrt
        c2 <- checkOp GetBidsByWallet getBidsByWalletCtrt
        c3 <- checkOp AddWalletItem addWalletItemCtrt
        c4 <- checkOp GetItemsByWallet getItemsByWalletCtrt
        d1 <- checkOp GetBalance getBalanceCtrt
        le <- return $ (ListE::[Exp] -> Exp)
                [a1,a2,a3,a4,a5,a6,a7,b1,b2,b3,b4,b5,b7,c1,c2,c3,c4,d1]
        t2 <- runIO getCurrentTime
        _ <- runIO $ putStrLn $ "----------------------------------------------------------"
        _ <- runIO $ putStrLn $ "Classification of operation contracts completed in "++
                  (show $ diffUTCTime t2 t1)++"."
        _ <- runIO $ putStrLn $ "----------------------------------------------------------"
        _ <- runIO $ hFlush stdout
        return le)


dtLib = do
    return $ mkDtLib
              [(StockItem, mkGenOp stockItem summarize, stockItemCtrtA),
               (RemoveFromStock, mkGenOp removeFromStock summarize, removeFromStockCtrtA),
               (UpdateMaxBid, mkGenOp updateMaxBid summarize, updateMaxBidCtrtA),
               (ShowItem, mkGenOp showItem summarize, showItemCtrtA),

               (GetBalance, mkGenOp getBalance summarize, getBalanceCtrtA),
               (DepositToWallet, mkGenOp depositToWallet summarize, depositToWalletCtrtA),
               (WithdrawFromWallet, mkGenOp withdrawFromWallet summarize, withdrawFromWalletCtrtA),

               (AddBid, mkGenOp addBid summarize, addBidCtrtA),
               (CancelBid, mkGenOp cancelBid summarize, cancelBidCtrtA),
               (GetBid, mkGenOp getBid summarize, getBidCtrtA),

               (AddItemBid, mkGenOp addItemBid summarize, addItemBidCtrtA),
               (RemoveItemBid, mkGenOp removeItemBid summarize, removeItemBidCtrtA),
               (GetBidsByItem, mkGenOp getBidsByItem summarize, getBidsByItemCtrtA),

               (AddWalletBid, mkGenOp addWalletBid summarize, addWalletBidCtrtA),
               (RemoveWalletBid, mkGenOp removeWalletBid summarize, removeWalletBidCtrtA),
               (GetBidsByWallet, mkGenOp getBidsByWallet summarize, getBidsByWalletCtrtA),

               (AddWalletItem, mkGenOp addWalletItem summarize, addWalletItemCtrtA),
               (GetItemsByWallet, mkGenOp getItemsByWallet summarize, getItemsByWalletCtrtA)]

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
      dtLib <- dtLib
      runShimNode dtLib [("localhost","9042")] keyspace ns
    Client -> do
      let threads = read $ numAuctions args
      let buyers = read $ numBuyers args
      mv::(MVar (Double, NominalDiffTime)) <- newEmptyMVar
      replicateM_ threads $ forkIO $ do
        mvarList <- replicateM buyers $ newEmptyMVar
        handShakeMVar <- newEmptyMVar
        mapM_ (\mv -> liftIO $ forkIO $ runSessionWithStats ns $ do
                        liftIO $ putStrLn "Buyer: started..."
                        liftIO $ putMVar handShakeMVar ()
                        liftIO $ putStrLn "Buyer: hand shake done..."
                        buyerCore mv $ read $ delayReq args) mvarList
        runSessionWithStats ns $ do
          liftIO $ putStrLn "Seller: started..."
          liftIO $ replicateM_ buyers $ liftIO $ takeMVar handShakeMVar
          liftIO $ putStrLn "Seller: hand shake done..."
          wid <- newWallet 0
          res <- sellItems mvarList wid (read $ numItems args)
          liftIO $ putMVar mv res
      statList <- replicateM threads $ takeMVar mv
      let (tpList, latList) = unzip statList
      putStrLn $ "Throughput (ops/s) = " ++ (show $ sum tpList)
      putStrLn $ "Avg. Latency (s) = " ++ (show $ sum latList / (fromIntegral $ length latList))
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
                        ++ " --numAuctions " ++ (numAuctions args)
                        ++ " --numItems " ++ (numItems args)
                        ++ " --delayReq " ++ (delayReq args)
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


data Message = NewItem ItemID | Terminate (MVar (Double, NominalDiffTime))

tryWinItem :: WalletID
           -> ItemID
           -> Int             -- Wait between successful bids
           -> Int             -- My current max bid
           -> Int             -- My max price willing to pay
           -> CSN (Bool, Int) -- True = Won! Int = maxBid
tryWinItem wid iid waitBetweenSuccessfulBids currentBid maxPrice = do
  delta <- liftIO $ randomIO
  let newBidAmt = currentBid + delta `mod` maxDelta
  if newBidAmt > maxPrice
  then do
    debugPrint $ show (iid,wid) ++ ": BeyondMaxPrice"
    waitTillAuctionEnd currentBid
  else do
    bidResult <- bidForItem wid iid newBidAmt
    case bidResult of
      ItemNotYetAvailable -> do
        debugPrint $ show (iid,wid) ++ ": ItemNotYetAvailable"
        liftIO $ threadDelay waitBetweenSuccessfulBids
        tryWinItem wid iid waitBetweenSuccessfulBids currentBid maxPrice
      ItemGone -> do
        debugPrint $ show (iid,wid) ++ ": ItemGone"
        res <- amIMaxBidder iid wid
        return $ (res, currentBid)
      OutBid -> do
        debugPrint $ show (iid,wid) ++ ": OutBid"
        tryWinItem wid iid waitBetweenSuccessfulBids newBidAmt maxPrice
      BidSuccess bid -> do
        debugPrint $ show (iid,wid) ++ ": BidSuccess"
        bideTime newBidAmt
  where
    waitTillAuctionEnd currentBid = do
      liftIO $ threadDelay waitBetweenSuccessfulBids
      item <- getItem iid
      case item of
        ItemRemoved -> do
          res <- amIMaxBidder iid wid
          return (res, currentBid)
        otherwise -> waitTillAuctionEnd currentBid
    bideTime newBidAmt = do
      liftIO $ threadDelay waitBetweenSuccessfulBids
      -- Woken up. Check if the auction is still running.
      res <- getItem iid
      iAmMaxBidder <- amIMaxBidder iid wid
      case res of
        ItemRemoved -> return (iAmMaxBidder, newBidAmt)
        otherwise -> -- Auction in progress
          if iAmMaxBidder
          then bideTime newBidAmt
          else tryWinItem wid iid waitBetweenSuccessfulBids newBidAmt maxPrice

buyerCore :: MVar Message -> Int -> CSN ()
buyerCore sellerMVar waitBetweenSuccessfulBids = do
  wid <- newWallet 10000000 -- Start with 10 MILLION dollars!
  msg <- liftIO $ takeMVar sellerMVar
  case msg of
    NewItem iid -> do
      debugPrint $ show (iid,wid) ++ ": NewItem"
      scale <- liftIO $ randomIO >>= \i -> return $ 2 + (i `mod` 10)
      (res, bid) <- tryWinItem wid iid waitBetweenSuccessfulBids minItemPrice (scale * minItemPrice)
      let WalletID widInt = wid
      let ItemID iidInt = iid
      if res
      then liftIO $ putStrLn $ "Buyer (" ++ show widInt ++ ") won  " ++ show iidInt
      else liftIO $ putStrLn $ "Buyer (" ++ show widInt ++ ") lost " ++ show iidInt
      buyerCore sellerMVar waitBetweenSuccessfulBids
    Terminate mv -> do
      stats <- getStats
      liftIO $ putMVar mv stats


sellItems :: [MVar Message] -> WalletID -> Int -> CSN (Double, NominalDiffTime)
sellItems buyerList wid 0 = do
  statMVList <- mapM (\m -> liftIO $ do
                              mv <- newEmptyMVar
                              putMVar m $ Terminate mv
                              return mv) buyerList
  buyerStatsList <- mapM (\m -> liftIO $ takeMVar m) statMVList
  myStats <- getStats
  let statsList = myStats:buyerStatsList
  let (tpList, latList) = unzip statsList
  return $ (sum tpList, sum latList / (fromIntegral $ length latList))
sellItems buyerList wid numItems = do
  iidInt <- liftIO $ randomIO
  let iid = ItemID $ iidInt
  let desc = show $ iid
  openAuction wid iid desc minItemPrice
  liftIO $ putStrLn $ "Auction opened: ItemID = " ++ (show iid)
  mapM_ (\m -> liftIO $ putMVar m $ NewItem iid) buyerList
  liftIO $ threadDelay auctionTime
  maxBidAmt <- concludeAuction wid iid
  liftIO $ putStrLn $ "Sold: " ++ (show iid) ++ " price = " ++ (show maxBidAmt)
  sellItems buyerList wid $ numItems - 1

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Run the Rubis benchmark"
     <> header "Rubis Benchmark" )
