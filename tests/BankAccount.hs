{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

import Database.Cassandra.CQL as CQL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize as S
import Data.Word (Word8)
import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>))
import System.Process (runCommand, terminateProcess)
import System.Environment (getExecutablePath, getArgs)

import Codeec.Types
import Codeec.Shim
import Codeec.Client
import Codeec.Marshall
import Codeec.NameService.SimpleBroker
import Codeec.TH
import Codeec.Contract

data BankAccount = Deposit_ Int | Withdraw_ Int | GetBalance_ deriving Show

instance Serialize BankAccount where
  put (Deposit_ v) = putTwoOf S.put S.put (0::Word8, v)
  put (Withdraw_ v) = putTwoOf S.put S.put (1::Word8, v)
  put (GetBalance_) = error "serializing GetBalance"
  get = do
    (i::Word8,v::Int) <- getTwoOf S.get S.get
    case i of
      0 -> return $ Deposit_ v
      1 -> return $ Withdraw_ v
      otherwise -> error "deserializing GetBalance"

instance CasType BankAccount where
  getCas = do
    r <- decode . unBlob <$> getCas
    case r of
      Left _ -> error "Parse fail"
      Right v -> return $ v
  putCas = putCas . Blob . encode
  casType _ = CBlob

instance Storable BankAccount where

type Res a = (a, Maybe BankAccount)

deposit :: [BankAccount] -> Int -> Res ()
deposit _ amt = ((), Just $ Deposit_ amt)

withdraw :: [BankAccount] -> Int -> Res Bool
withdraw ctxt amt =
  let (bal, _) = getBalance ctxt ()
  in if bal > amt
     then (True, Just $ Withdraw_ amt)
     else (False, Nothing)

getBalance :: [BankAccount] -> () -> Res Int
getBalance ops () =
  let v = foldl acc 0 ops
  in (v, Nothing)
  where
    acc s (Deposit_ i) = s + i
    acc s (Withdraw_ i) = s - i
    acc s GetBalance_ = s


data Kind = B | C | S | D deriving (Read, Show)

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


mkOperations [''BankAccount]


main :: IO ()
main = do
  (kindStr:_) <- getArgs
  let k :: Kind = read kindStr
  case k of
    B -> startBroker (Frontend $ "tcp://*:" ++ show fePort)
                     (Backend $ "tcp://*:" ++ show bePort)
    S -> do
      let dtLib = mkDtLib [(Deposit, mkGen deposit, $(check  "Deposit" $ \x -> liftProp $ (true_ :: Prop ())))]
      runShimNode dtLib (Backend $ "tcp://localhost:" ++ show bePort) 5560
    C -> do
      sess <- beginSession $ Frontend $ "tcp://localhost:" ++ show fePort

      putStrLn "Client : performing deposit"
      r::() <- invoke sess Deposit (100::Int)

      putStrLn "Client : performing withdraw"
      r::(Maybe Int) <- invoke sess Withdraw (10::Int)
      putStrLn $ show r

      putStrLn "Client : performing getBalance"
      r::Int <- invoke sess GetBalance ()
      putStrLn $ show r
      endSession sess
    D -> do
      progName <- getExecutablePath
      putStrLn "Driver : Starting broker"
      b <- runCommand $ progName ++ " B"
      putStrLn "Driver : Starting server"
      s <- runCommand $ progName ++ " S"
      putStrLn "Driver : Starting client"
      c <- runCommand $ progName ++ " C"
      threadDelay 5000000
      mapM_ terminateProcess [b,s,c]

