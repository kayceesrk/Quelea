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
import Data.Int (Int64)
import Data.Time.Clock
import qualified Data.Map as M

import ShoppingListDefs hiding (renameItem, deleteItem, viewList)
import qualified ShoppingListDefs as S

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


data Kind = B | C | S | D | Drop deriving (Read, Show)

keyspace :: Keyspace
keyspace = Keyspace $ pack "ShoppingList"

dtLib = mkDtLib [(ChangeQuantity, mkGenOp changeQuantity summarize, $(checkOp ChangeQuantity changeQuantityCtrt)),
                 (RenameItem, mkGenOp S.renameItem summarize, $(checkOp RenameItem renameItemCtrt)),
                 (DeleteItem, mkGenOp S.deleteItem summarize, $(checkOp DeleteItem deleteItemCtrt)),
                 (ViewList, mkGenOp S.viewList summarize, $(checkOp ViewList viewListCtrt))]

newItem :: Key -> ItemName -> Quantity -> CSN ()
newItem k n q = do
  ts <- liftIO $ getCurrentTime
  invoke k ChangeQuantity (n,q,ts)

renameItem :: Key -> ItemName -> ItemName -> CSN Bool
renameItem k oldName newName = do
  ts <- liftIO $ getCurrentTime
  invoke k RenameItem (oldName, newName, ts)

deleteItem :: Key -> ItemName -> CSN ()
deleteItem k name = do
  ts <- liftIO $ getCurrentTime
  invoke k DeleteItem (name, ts)

viewList :: Key -> CSN (M.Map ItemName Quantity)
viewList k = invoke k ViewList ()

increaseQuantity :: Key -> ItemName -> Quantity -> CSN ()
increaseQuantity = newItem

decreaseQuantity :: Key -> ItemName -> Quantity -> CSN ()
decreaseQuantity k n q = newItem k n (-q)

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

    C -> do
      runSession (Frontend $ "tcp://localhost:" ++ show fePort) $ do
        key <- liftIO $ newKey
        newItem key "Milk" 1
        newItem key "Eggs" 12
        renameItem key "Eggs" "Large Eggs"
        deleteItem key "Milk"
        newItem key "Bread" 1
        increaseQuantity key "Bread" 2
        decreaseQuantity key "Bread" 4
        increaseQuantity key "Bread" 5
        res <- viewList key
        liftIO . putStrLn $ "result " ++ show res
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
