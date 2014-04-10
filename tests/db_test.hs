{-# LANGUAGE OverloadedStrings, DataKinds, TypeSynonymInstances, FlexibleInstances, BangPatterns, ScopedTypeVariables #-}

import Codeec
import Database.Cassandra.CQL as CQL
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Text (Text, pack)
import Control.Monad.Trans (liftIO)
import System.Random
import Control.Applicative

type BankAccount = Int
instance Effect BankAccount

deposit :: Ctxt Int -> Int -> Proc Int ()
deposit _ amt = effect amt

getBalance :: Ctxt Int -> () -> Proc Int Int
getBalance ops () = do
  let v = foldl acc 0 $ labNodes ops
  performIO $ print v
  return v
  where
    acc s (_::Int, i) = s + i
--    acc s (_::Int, Withdraw i) = s - i

main = do
  pool <- newPool [("localhost", "9042")] "test"
  x <- randomIO
  runEC pool $ do
    createTable "BankAccount"
    performOp deposit "BankAccount" x 100
    performOp deposit "BankAccount" x 200
    performOp getBalance "BankAccount" x ()
    liftIO $ print "HERE--222"

