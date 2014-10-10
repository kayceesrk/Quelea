{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module MicroBlogTxns (
  addNewUser,
  getPassword,
  followUser,
  newTweet,
  getUserline,
) where

import Codeec.ClientMonad
import Codeec.TH (checkTxn)

import MicroBlogDefs
import MicroBlogCtrts

import Control.Monad.Trans (liftIO)
import Data.Time.Clock
import System.Random (randomIO)
import Control.Applicative ((<$>))

type Username = String
type Password = String

addNewUser :: UserID -> Username -> Password -> CSN Bool
addNewUser uid uname pwd = atomically ($(checkTxn "_addNewUserTxn" addNewUserTxnCtrt)) $ do
  r::Bool <- invoke (mkKey uname) AddUsername uid
  if not r
  then return False {- username has already been taken -}
  else do {- success -}
    r::() <- invoke (mkKey uid) AddUser (uname,pwd)
    return True

-- Returns (Just pwd) on Success
getPassword :: Username -> CSN (Maybe Password)
getPassword uname = atomically ($(checkTxn "_getPasswordTxn" getPasswordTxnCtrt)) $ do
  mbUid::Maybe UserID <- invoke (mkKey uname) GetUserID ()
  case mbUid of
    Nothing -> return Nothing
    Just uid -> do
      Just (_::String, pwd) <- invoke (mkKey uid) GetUserInfo ()
      return $ Just pwd

-- Returns True on Success
followUser :: Username -> Username -> CSN Bool
followUser me target = atomically ($(checkTxn "_followUserTxn" followUserTxnCtrt)) $ do
  mbMyUid::Maybe UserID <- invoke (mkKey me) GetUserID ()
  case mbMyUid of
    Nothing -> return False
    Just myUid -> do
      mbTargetUid::Maybe UserID <- invoke (mkKey target) GetUserID ()
      case mbTargetUid of
        Nothing -> return False
        Just targetUid -> do
          _::() <- invoke (mkKey myUid) AddFollowing targetUid
          _::() <- invoke (mkKey targetUid) AddFollower myUid
          return True

type Tweet = String

newTweet :: UserID -> String -> CSN ()
newTweet uid tweet = do
  timestamp <- liftIO $ getCurrentTime
  tweetID <- liftIO $ TweetID <$> randomIO
  -- Add into Tweet table
  r::() <- invoke (mkKey tweetID) NewTweet (uid, take 140 tweet, timestamp)
  -- Add to userline

  followers::[UserID] <- invoke (mkKey uid) GetFollowers ()
  flip mapM_ followers $ \follower -> do
    _::() <- invoke (mkKey follower) NewTweetTL (timestamp, tweetID)
    return ()

getUserline :: UserID -> CSN [(String, UTCTime)]
getUserline = undefined

getTimeline :: UserID -> CSN [(UserID, String, UTCTime)]
getTimeline = undefined
