{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Quelea.Shim
import Quelea.ClientMonad
import Quelea.Marshall
import Quelea.NameService.Types
import Quelea.NameService.SimpleBroker
import Quelea.TH

import System.Process (runCommand, terminateProcess)
import System.Environment (getExecutablePath, getArgs)
import Database.Cassandra.CQL
import Control.Monad.Trans (liftIO)
import Data.Text (pack)
import Quelea.Types (summarize)
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)

import ShoppingCartDefs
import ShoppingCartCtrts
import ShoppingCartTxns

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


data Kind = B | C | S | D | Drop deriving (Read, Show)

keyspace :: Keyspace
keyspace = Keyspace $ pack "ShoppingCart"

dtLib = mkDtLib [(StockItem, mkGenOp stockItem summarize, $(checkOp StockItem stockItemCtrt)),
                 (AddToStock, mkGenOp addToStock summarize, $(checkOp AddToStock addToStockCtrt)),
                 (RemoveFromStock, mkGenOp removeFromStock summarize, $(checkOp RemoveFromStock removeFromStockCtrt)),
                 (AlterPrice, mkGenOp alterPrice summarize, $(checkOp AlterPrice alterPriceCtrt)),
                 (ShowItem, mkGenOp showItem summarize, $(checkOp ShowItem showItemCtrt)),
                 (AddCart, mkGenOp addCart summarize, $(checkOp AddCart addCartCtrt)),
                 (RemoveCart, mkGenOp removeCart summarize, $(checkOp RemoveCart removeCartCtrt)),
                 (AddItemsToCart, mkGenOp addItemsToCart summarize, $(checkOp AddItemsToCart addItemsToCartCtrt)),
                 (RemoveItemsFromCart, mkGenOp removeItemsFromCart summarize, $(checkOp RemoveItemsFromCart removeItemsFromCartCtrt)),
                 (GetCartSummary, mkGenOp getCartSummary summarize, $(checkOp GetCartSummary getCartSummaryCtrt))]

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
      key <- liftIO $ newKey
      r::() <- invoke key StockItem ("Organic Milk",2::Int, 20::Int)
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
      return ()
      {-threadDelay 5000000
      mapM_ terminateProcess [b,s,c]
      runCas pool $ dropTables-}

    Drop -> do
      pool <- newPool [("localhost", "9042")] keyspace Nothing
      runCas pool $ dropTables
