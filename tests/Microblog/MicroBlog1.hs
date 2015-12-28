{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Quelea.Shim
import Quelea.ClientMonad
import Quelea.Marshall
import Quelea.NameService.Types
import Quelea.NameService.SimpleBroker
import Quelea.TH

import Language.Haskell.TH 
import Language.Haskell.TH.Syntax (Exp (..))
-- import System.Process (runCommand, terminateProcess)
import System.Posix.Process (forkProcess)
import System.Posix.Signals (signalProcess, killProcess, internalAbort)
import System.Environment (getExecutablePath, getArgs)
import Database.Cassandra.CQL
import Control.Monad.Trans (liftIO)
import Data.Text (pack)
import Quelea.Types (summarize)
import Control.Monad (replicateM_, void, ap)
import Control.Concurrent (threadDelay)
import Data.Time
import System.IO (hFlush, stdout)
import Data.Serialize

{- For user input parsing -}
import Text.Parsec
import Text.Parsec.String
import Data.Char (isLetter, isDigit)
import Control.Exception

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

{- Removing typesig for the following results in ambiguous type error. -}
invokeOp :: Serialize a => Operation -> a -> CSN ()
invokeOp op arg = do
  key <- liftIO $ newKey
  invoke key op arg

whitespace :: Parser ()
whitespace = void $ many $ char ' '

varW :: Parser String
varW = do
  whitespace
  fc <- firstChar
  rest <- many nonFirstChar
  return (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

stringW :: String -> Parser String
stringW s = (whitespace >> string s)

{-
opParser :: Parser Operation
opParser = 
      (stringW "AddUser" >> return AddUser)
  <|> (stringW "NewTweet" >> return NewTweet)-}

csnParser :: Parser (CSN ())
csnParser = 
  let addUserParser = do
        void $ stringW "AddUser"
        uname <- varW
        pwd <- varW
        return $ invokeOp AddUser (uname,pwd)
      newTweetParser = do
        void $ stringW "NewTweet"
        owner <- varW
        txt <- (whitespace >> (many anyChar))
        return $ invokeOp NewTweet (owner,txt)
  in addUserParser <|> newTweetParser

interactiveSession :: CSN ()
interactiveSession =  do
  liftIO $ putStrLn "What would you like to do?"
  line <- liftIO $ getLine
  let es = parse (stringW "EndSession") "EOF" line
  let x = case es of 
              Left _ -> ()
              Right _ -> throw UserInterrupt
  liftIO $ evaluate x
  let csnHead = case parse csnParser "Operation Parser" line of 
              Left _ -> return ()
              Right csn -> csn
  let csnTail = interactiveSession
  csnHead >> csnTail

runBroker :: IO ()
runBroker = 
  startBroker (Frontend $ "tcp://*:" ++ show fePort)
              (Backend $ "tcp://*:" ++ show bePort)
  
runServer :: NameService -> IO ()
runServer ns = do
  dtLib <- dtLib
  runShimNode dtLib [("localhost","9042")] keyspace ns

runClient :: NameService -> IO ()
runClient ns = catch (runSession ns interactiveSession) 
    (\(e::AsyncException) -> return ())
  
main :: IO ()
main = do
  (kindStr:broker:restArgs) <- getArgs
  let k = read kindStr
  let ns = mkNameService (Frontend $ "tcp://" ++ broker ++ ":" ++ show fePort)
                         (Backend  $ "tcp://" ++ broker ++ ":" ++ show bePort)
                         "localhost" 5560
  case k of
    B -> runBroker

    S -> runServer ns

    C -> runClient ns

    D -> do
      pool <- newPool [("localhost","9042")] keyspace Nothing
      runCas pool $ createTables
      putStrLn "Driver : Starting broker"
      bpid <- forkProcess $ runBroker
      putStrLn "Driver : Starting server"
      spid <- forkProcess $ runServer ns 
      putStrLn "Driver : Starting client"
      cpid <- forkProcess $ runClient ns
      threadDelay 25000000
      let terminateProcess = signalProcess internalAbort
      mapM_ terminateProcess [bpid,spid,cpid]
      runCas pool $ dropTables

    Drop -> do
      pool <- newPool [("localhost", "9042")] keyspace Nothing
      runCas pool $ dropTables
