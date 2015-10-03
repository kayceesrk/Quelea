{-# Language DataKinds, ScopedTypeVariables, OverloadedStrings #-}

import Database.Cassandra.CQL
import Data.Text
import Control.Monad.Trans (liftIO)

mkQuery :: Int -> Int -> Int -> Cas Bool
mkQuery a b c =
  let qs :: Query Write (Int, Int, Int) () = query $ pack $ "insert into test (hash, seqno, value) values (?, ?, ?) if not exists"
  in executeTrans qs (a,b,c)

main = do
  pool <- newPool [("localhost", "9042")] "Quelea" Nothing
  runCas pool $ do
    r <- mkQuery 5 5 5
    liftIO . putStrLn $ show r
