{-# LANGUAGE OverloadedStrings, DataKinds, TypeSynonymInstances, FlexibleInstances, BangPatterns, ScopedTypeVariables #-}

import Codeec
import Database.Cassandra.CQL as CQL
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Text (Text, pack)
import Control.Monad.Trans (liftIO)
import System.Random
import Control.Applicative
import Data.Serialize as S
import Data.Word (Word8)

data BankAccount = Deposit Int | Withdraw Int deriving Show

instance Serialize BankAccount where
  put (Deposit v) = putTwoOf S.put S.put (0::Word8, v)
  put (Withdraw v) = putTwoOf S.put S.put (1::Word8, v)
  get = do
    (i::Word8,v::Int) <- getTwoOf S.get S.get
    case i of
      0 -> return $ Deposit v
      1 -> return $ Withdraw v

instance CasType BankAccount where
  getCas = do
    r <- decode . unBlob <$> getCas
    case r of
      Left _ -> error "Parse fail"
      Right v -> return $ v
  putCas = putCas . Blob . encode
  casType _ = CBlob

instance Storable BankAccount

type Res a = (a, Maybe BankAccount)

deposit :: Ctxt BankAccount -> Int -> Res ()
deposit _ amt = ((), Just $ Deposit amt)

withdraw :: Ctxt BankAccount -> Int -> Res Bool
withdraw ctxt amt =
  let (bal, _) = getBalance ctxt ()
  in if bal > amt
     then (True, Just $ Withdraw amt)
     else (False, Nothing)

getBalance :: Ctxt BankAccount -> () -> Res Int
getBalance ops () =
  let v = foldl acc 0 $ labNodes ops
  in (v, Nothing)
  where
    acc s (_::Int, (_,_,Deposit i)) = s + i
    acc s (_::Int, (_,_,Withdraw i)) = s - i

foo :: Ctxt BankAccount -> Ctxt BankAccount
foo = id

main = do
  pool <- newPool [("localhost", "9042")] "test"
  x <- randomIO
  runEC pool $ do
    createTable "BankAccount"

    mkEC deposit "BankAccount" x 100
    v <- mkEC getBalance "BankAccount" x ()
    liftIO $ putStrLn $ "After deposit 100. Balance = " ++ show v

    printCtxt "BankAccount" x foo
    liftIO $ putStrLn ""

    mkEC deposit "BankAccount" x 200
    v <- mkEC getBalance "BankAccount" x ()
    liftIO $ putStrLn $ "After deposit 200. Balance = " ++ show v

    printCtxt "BankAccount" x foo
    liftIO $ putStrLn ""

    s <- mkEC withdraw "BankAccount" x 200
    v <- mkEC getBalance "BankAccount" x ()
    liftIO $ putStrLn $ "After withdraw 200. Success? = "++ show s ++ ". Balance = " ++ show v

    printCtxt "BankAccount" x foo
    liftIO $ putStrLn ""

    s <- mkEC withdraw "BankAccount" x 200
    v <- mkEC getBalance "BankAccount" x ()
    liftIO $ putStrLn $ "After withdraw 200. Success? = "++ show s ++ ". Balance = " ++ show v

    printCtxt "BankAccount" x foo
    liftIO $ putStrLn ""

    -- dropTable "BankAccount"
