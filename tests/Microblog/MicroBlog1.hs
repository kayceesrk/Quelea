{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Quelea.Shim
import Quelea.ClientMonad
import Quelea.Marshall
import Quelea.NameService.Types
import Quelea.NameService.SimpleBroker
import Quelea.TH

import Language.Haskell.TH 
import Language.Haskell.TH.Syntax (Exp (..))
import System.Process (runCommand, terminateProcess)
import System.Environment (getExecutablePath, getArgs)
import Database.Cassandra.CQL
import Control.Monad.Trans (liftIO)
import Data.Text (pack)
import Quelea.Types (summarize)
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)
import Data.Time
import System.IO (hFlush, stdout)

import MicroBlogDefs
import MicroBlogCtrts
import MicroBlogTxns

fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559


data Kind = B | C | S | D | Drop deriving (Read, Show)

[
  addUserCtrtA,
  addUsernameCtrtA,
  getUserIDCtrtA,
  getUserInfoCtrtA,
  addFollowerCtrtA,
  remFollowerCtrtA,
  addFollowingCtrtA,
  remFollowingCtrtA,
  addBlocksCtrtA,
  addIsBlockedByCtrtA,
  getBlocksCtrtA,
  getIsBlockedByCtrtA,
  getFollowersCtrtA,
  getFollowingCtrtA,
  addTweetCtrtA,
  getTweetCtrtA,
  addToTimelineCtrtA,
  getTweetsInTimelineCtrtA,
  addToUserlineCtrtA,
  getTweetsInUserlineCtrtA ] = 
    $(do 
        t1 <- runIO getCurrentTime
        a <- (checkOp AddUser addUserCtrt)
        b <- (checkOp AddUsername addUsernameCtrt)
        c <- (checkOp GetUserID getUserIDCtrt)
        d <- (checkOp GetUserInfo getUserInfoCtrt)
        e <- (checkOp AddFollower addFollowerCtrt)
        f <- (checkOp RemFollower remFollowerCtrt)
        g <- (checkOp AddFollowing addFollowingCtrt)
        h <- (checkOp RemFollowing remFollowingCtrt)
        i <- (checkOp Blocks addBlocksCtrt)
        j <- (checkOp IsBlockedBy addIsBlockedByCtrt)
        k <- (checkOp GetBlocks getBlocksCtrt)
        l <- (checkOp GetIsBlockedBy getIsBlockedByCtrt)
        m <- (checkOp GetFollowers getFollowersCtrt)
        n <- (checkOp GetFollowing getFollowingCtrt)
        o <- (checkOp NewTweet addTweetCtrt)
        p <- (checkOp GetTweet getTweetCtrt)
        q <- (checkOp NewTweetTL addToTimelineCtrt)
        r <- (checkOp GetTweetsInTL getTweetsInTimelineCtrt)
        s <- (checkOp NewTweetUL addToUserlineCtrt)
        t <- (checkOp GetTweetsInUL getTweetsInUserlineCtrt)
        le <- return $ (ListE::[Exp] -> Exp) 
                [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t]
        t2 <- runIO getCurrentTime
        _ <- runIO $ putStrLn $ "----------------------------------------------------------"
        _ <- runIO $ putStrLn $ "Classification of operation contracts completed in "++
                  (show $ diffUTCTime t2 t1)++"."
        _ <- runIO $ putStrLn $ "----------------------------------------------------------"
        _ <- runIO $ hFlush stdout
        return le)

keyspace :: Keyspace
keyspace = Keyspace $ pack "MicroBlog"

dtLib = do
  return $ mkDtLib [(AddUser, mkGenOp addUser summarize, addUserCtrtA),
                    (AddUsername, mkGenOp addUsername summarize, addUsernameCtrtA),
                    (GetUserID, mkGenOp getUserID summarize, getUserIDCtrtA),
                    (GetUserInfo, mkGenOp getUserInfo summarize, getUserInfoCtrtA),
                    (AddFollower, mkGenOp addFollower summarize, addFollowerCtrtA),
                    (RemFollower, mkGenOp remFollower summarize, remFollowerCtrtA),
                    (AddFollowing, mkGenOp addFollowing summarize, addFollowingCtrtA),
                    (RemFollowing, mkGenOp remFollowing summarize, remFollowingCtrtA),
                    (Blocks, mkGenOp addBlocks summarize, addBlocksCtrtA),
                    (IsBlockedBy, mkGenOp addIsBlockedBy summarize, addIsBlockedByCtrtA),
                    (GetBlocks, mkGenOp getBlocks summarize, getBlocksCtrtA),
                    (GetIsBlockedBy, mkGenOp getIsBlockedBy summarize, getIsBlockedByCtrtA),
                    (GetFollowers, mkGenOp getFollowers summarize, getFollowersCtrtA),
                    (GetFollowing, mkGenOp getFollowing summarize, getFollowingCtrtA),
                    (NewTweet, mkGenOp addTweet summarize, addTweetCtrtA),
                    (GetTweet, mkGenOp getTweet summarize, getTweetCtrtA),
                    (NewTweetTL, mkGenOp addToTimeline summarize, addToTimelineCtrtA),
                    (GetTweetsInTL, mkGenOp getTweetsInTimeline summarize, getTweetsInTimelineCtrtA),
                    (NewTweetUL, mkGenOp addToUserline summarize, addToUserlineCtrtA),
                    (GetTweetsInUL, mkGenOp getTweetsInUserline summarize, getTweetsInUserlineCtrtA)]

main :: IO ()
main = do
  (kindStr:broker:restArgs) <- getArgs
  let k = read kindStr
  let ns = mkNameService (Frontend $ "tcp://" ++ broker ++ ":" ++ show fePort)
                         (Backend  $ "tcp://" ++ broker ++ ":" ++ show bePort) "localhost" 5560
  case k of
    B -> startBroker (Frontend $ "tcp://*:" ++ show fePort)
                     (Backend $ "tcp://*:" ++ show bePort)

    S -> do
      dtLib <- dtLib
      runShimNode dtLib [("localhost","9042")] keyspace ns

    C -> runSession ns $ do
      key <- liftIO $ newKey
      r::() <- invoke key AddUser ("Alice","test123")
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
      threadDelay 25000000
      mapM_ terminateProcess [b,s,c]
      runCas pool $ dropTables

    Drop -> do
      pool <- newPool [("localhost", "9042")] keyspace Nothing
      runCas pool $ dropTables
