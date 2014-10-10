{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module MicroBlogTxns (
  addNewUser,
  getPassword,
  followUser,
  newTweet,
  getUserline,
  getTimeline,
  getFollowersUN,
  getFollowingUN,
) where

import Codeec.ClientMonad
import Codeec.TH (checkTxn)

import MicroBlogDefs
import MicroBlogCtrts

import Control.Monad.Trans (liftIO)
import Data.Time.Clock
import System.Random (randomIO)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Control.Monad (foldM)

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

getFollowersUN :: Username -> CSN [Username]
getFollowersUN uname = do
  mbMyUid::Maybe UserID <- invoke (mkKey uname) GetUserID ()
  case mbMyUid of
    Nothing -> return []
    Just myUid -> do
      uidList::[UserID] <- invoke (mkKey myUid) GetFollowers ()
      foldM (\acc uid -> do
        userInfo::Maybe(String,String) <- invoke (mkKey uid) GetUserInfo ()
        case userInfo of
          Nothing -> return acc
          Just (followerName,_) -> return $ followerName:acc) [] uidList

getFollowingUN :: Username -> CSN [Username]
getFollowingUN uname = do
  mbMyUid::Maybe UserID <- invoke (mkKey uname) GetUserID ()
  case mbMyUid of
    Nothing -> return []
    Just myUid -> do
      uidList::[UserID] <- invoke (mkKey myUid) GetFollowing ()
      foldM (\acc uid -> do
        userInfo::Maybe(String,String) <- invoke (mkKey uid) GetUserInfo ()
        case userInfo of
          Nothing -> return acc
          Just (followerName,_) -> return $ followerName:acc) [] uidList

type Tweet = String

newTweet :: UserID -> String -> CSN ()
newTweet uid tweet = do
  timestamp <- liftIO $ getCurrentTime
  tweetID <- liftIO $ TweetID <$> randomIO
  -- Add into Tweet table
  r::() <- invoke (mkKey tweetID) NewTweet (uid, take 140 tweet, timestamp)
  newTweetEffect <- S.singleton . fromJust <$> getLastEffect
  -- Add to userline
  atomicallyWith newTweetEffect ($(checkTxn "_addToUserlineTxn" addToUserlineTxnCtrt)) $ do
    _::() <- invoke (mkKey uid) NewTweetUL (timestamp, tweetID)
    return ()
  -- Add to timeline
  followers::[UserID] <- invoke (mkKey uid) GetFollowers ()
  flip mapM_ followers $ \follower -> do
    atomicallyWith newTweetEffect ($(checkTxn "_addToTimelineTxn" addToTimelineTxnCtrt)) $ do
      _::() <- invoke (mkKey follower) NewTweetTL (timestamp, tweetID)
      return ()

getUserline :: UserID -> UTCTime -> UTCTime -> CSN [(String, UTCTime)]
getUserline uid beginTime endTime = atomically ($(checkTxn "_getUserlineTxn" getUserlineTxnCtrt)) $ do
  tweetInfoList::[(UTCTime, TweetID)] <- invoke (mkKey uid) GetTweetsInUL ()
  let filteredInfo = filter (\(t,_) -> t >= beginTime && t <= endTime) tweetInfoList
  flip mapM filteredInfo $ \(t,tid) -> do
    Just (uid::UserID, tweet::String, _::UTCTime) <- invoke (mkKey tid) GetTweet ()
    return (tweet, t)

getTimeline :: UserID -> UTCTime -> UTCTime -> CSN [(TweetID, UserID, String, UTCTime)]
getTimeline uid beginTime endTime = atomically ($(checkTxn "_getTimelineTxn" getTimelineTxnCtrt)) $ do
  tweetInfoList::[(UTCTime, TweetID)] <- invoke (mkKey uid) GetTweetsInTL ()
  let filteredInfo = filter (\(t,_) -> t >= beginTime && t <= endTime) tweetInfoList
  flip mapM filteredInfo $ \(t,tid) -> do
    Just (uid::UserID, tweet::String, _::UTCTime) <- invoke (mkKey tid) GetTweet ()
    return (tid, uid, tweet, t)
