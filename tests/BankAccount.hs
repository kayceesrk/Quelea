{-# LANGUAGE ScopedTypeVariables #-}

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

data BAEffect = Deposit Int | Withdraw Int deriving Show

instance Serialize BAEffect where
  put (Deposit v) = putTwoOf S.put S.put (0::Word8, v)
  put (Withdraw v) = putTwoOf S.put S.put (1::Word8, v)
  get = do
    (i::Word8,v::Int) <- getTwoOf S.get S.get
    case i of
      0 -> return $ Deposit v
      1 -> return $ Withdraw v

instance CasType BAEffect where
  getCas = do
    r <- decode . unBlob <$> getCas
    case r of
      Left _ -> error "Parse fail"
      Right v -> return $ v
  putCas = putCas . Blob . encode
  casType _ = CBlob

instance Storable BAEffect where

type Res a = (a, Maybe BAEffect)

deposit :: [BAEffect] -> Int -> Res ()
deposit _ amt = ((), Just $ Deposit amt)

withdraw :: [BAEffect] -> Int -> Res Bool
withdraw ctxt amt =
  let (bal, _) = getBalance ctxt ()
  in if bal > amt
     then (True, Just $ Withdraw amt)
     else (False, Nothing)

getBalance :: [BAEffect] -> () -> Res Int
getBalance ops () =
  let v = foldl acc 0 ops
  in (v, Nothing)
  where
    acc s (Deposit i) = s + i
    acc s (Withdraw i) = s - i


data Kind = B | C | S | D deriving (Read, Show)

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559

main :: IO ()
main = do
  (kindStr:_) <- getArgs
  let k :: Kind = read kindStr
  case k of
    B -> startBroker (Frontend $ "tcp://*:" ++ show fePort)
                     (Backend $ "tcp://*:" ++ show bePort)
    S -> do
      let dtLib = Map.fromList [(ObjType "BankAccount",
                    Map.fromList [(OperName "deposit",  (mkGeneric deposit, Un)),
                                  (OperName "withdraw", (mkGeneric withdraw, Un)),
                                  (OperName "getBalance", (mkGeneric getBalance, Un))])]
      runShimNode dtLib (Backend $ "tcp://localhost:" ++ show bePort) 5560
    C -> do
      sess <- beginSession $ Frontend $ "tcp://localhost:" ++ show fePort

      putStrLn "Client : performing deposit"
      r::() <- invoke sess "BankAccount" "deposit" (100::Int)

      putStrLn "Client : performing withdraw"
      r::(Maybe Int) <- invoke sess "BankAccount" "withdraw" (10::Int)
      putStrLn $ show r

      putStrLn "Client : performing getBalance"
      r::Int <- invoke sess "BankAccount" "getBalance" ()
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

