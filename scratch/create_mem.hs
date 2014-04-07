{-# LANGUAGE OverloadedStrings, DataKinds, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}

import Database.Cassandra.CQL
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import System.Random
import Data.Serialize
import Control.Applicative
import Data.Either.Unwrap
import Data.Set as S
import System.IO.Unsafe
import qualified Data.UUID as U
import qualified Data.ByteString.Internal as BS
import Data.Maybe

data Link = Link UUID UUID deriving (Eq, Ord, Read, Show)

instance CasType Link where
  putCas (Link x y) = do
    putLazyByteString $ U.toByteString x
    putLazyByteString $ U.toByteString y
  getCas = do
    x <- fromJust . U.fromByteString <$> getLazyByteString 16
    y <- fromJust . U.fromByteString <$> getLazyByteString 16
    return $ Link x y
  casType _ = CBlob

createMemory :: Query Schema () ()
createMemory = "create table memory\
                  \ (key uuid,\
                  \ sess uuid,\
                  \ at uuid,\
                  \ event text,\
                  \ value int,\
                  \ vis set<blob>,\
                  \ primary key (key, sess, at))"


insertData :: Query Write (UUID, UUID, UUID, Text, Int, S.Set Link) ()
insertData = "insert into memory (key, sess, at, event, value, vis) values (?, ?, ?, ?, ?, ?)"

readData :: Query Rows () (UUID, UUID, UUID, Text, Int, S.Set Link)
readData = "select * from memory"

main = do
    pool <- newPool [("localhost", "9042")] "test" -- servers, keyspace
    runCas pool $ do
      liftIO . print =<< executeSchema QUORUM createMemory ()
      x <- liftIO randomIO
      y <- liftIO randomIO
      executeWrite QUORUM insertData (x, x, y, "Deposit", 10, S.fromList [Link x y, Link y x])

      dataList <- executeRows QUORUM readData ()
      liftIO $ forM_ dataList $ \(key, sess, at, event, value, vis) -> do
        putStrLn ""
        putStrLn $ "id    : "++ show key
        putStrLn $ "sess  : "++ show sess
        putStrLn $ "at    : "++ show at
        putStrLn $ "event : "++ show event
        putStrLn $ "value : "++ show value
        putStrLn $ "vis   : "++ show vis


