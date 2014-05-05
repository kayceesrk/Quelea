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

instance Effect BankAccount

deposit :: Ctxt BankAccount -> Int -> Proc BankAccount ()
deposit _ amt = effect $ Deposit amt

withdraw :: Ctxt BankAccount -> Int -> Proc BankAccount Bool
withdraw ctxt amt = do
  bal <- getBalance ctxt ()
  if bal > amt
  then do
    effect $ Withdraw amt
    return True
  else do
    return False

getBalance :: Ctxt BankAccount -> () -> Proc BankAccount Int
getBalance ops () = do
  let v = foldl acc 0 $ labNodes ops
  return v
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

    doProc deposit "BankAccount" x 100
    v <- doProc getBalance "BankAccount" x ()
    liftIO $ putStrLn $ "After deposit 100. Balance = " ++ show v

    printCtxt "BankAccount" x foo
    liftIO $ putStrLn ""

    doProc deposit "BankAccount" x 200
    v <- doProc getBalance "BankAccount" x ()
    liftIO $ putStrLn $ "After deposit 200. Balance = " ++ show v

    printCtxt "BankAccount" x foo
    liftIO $ putStrLn ""

    s <- doProc withdraw "BankAccount" x 200
    v <- doProc getBalance "BankAccount" x ()
    liftIO $ putStrLn $ "After withdraw 200. Success? = "++ show s ++ ". Balance = " ++ show v

    printCtxt "BankAccount" x foo
    liftIO $ putStrLn ""

    s <- doProc withdraw "BankAccount" x 200
    v <- doProc getBalance "BankAccount" x ()
    liftIO $ putStrLn $ "After withdraw 200. Success? = "++ show s ++ ". Balance = " ++ show v

    printCtxt "BankAccount" x foo
    liftIO $ putStrLn ""

    -- dropTable "BankAccount"
