{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module MicroBlogDefs (
  UserEffect(..),
) where


import Database.Cassandra.CQL as CQL
import Data.Serialize as S
import Data.Time.Clock
import Data.UUID
import Control.Applicative ((<$>))

import Codeec.Types
import Codeec.Contract
import Codeec.TH

--------------------------------------------------------------------------------
-- User table : key = UserID

newtype UserID = UserID UUID
data UserEffect = NewUser_ {username :: String, password :: String}
                | AddFollowing_ {follows :: UserID}
                | AddFollower_ {followedBy :: UserID}

instance Serialize UserID where
  put (UserID uuid) = put uuid
  get = UserID <$> get

instance Serialize UserEffect where
  put (NewUser_ x y) = putWord8 0 >> put x >> put y
  put (AddFollowing_ x) = putWord8 1 >> put x
  put (AddFollower_ x) = putWord8 2 >> put x
  get = do
    i <- getWord8
    case i of
      0 -> do
        x <- get
        y <- get
        return $ NewUser_ x y
      1 -> AddFollowing_ <$> get
      2 -> AddFollower_ <$> get

--------------------------------------------------------------------------------
-- Username table : Key = String

data UsernameEffect = NewUsername_ UserID

instance Serialize UsernameEffect where
  put (NewUsername_ uid) = put uid
  get = NewUsername_ <$> get

--------------------------------------------------------------------------------
-- Tweet table : Key = TweetID

newtype TweetID = TweetID UUID
data TweetEffect = NewTweet_ UserID String UTCTime

instance Serialize TweetID where
  put (TweetID tid) = put tid
  get = TweetID <$> get

instance Serialize UTCTime where
  put t = put $ show t
  get = read <$> get

instance Serialize TweetEffect where
  put (NewTweet_ x y z) = put x >> put y >> put z
  get = do
    x <- get
    y <- get
    z <- get
    return $ NewTweet_ x y z

--------------------------------------------------------------------------------
-- UserLine table : Key = UserID

data UserlineEffect = NewTweetUL_ UTCTime String

instance Serialize UserlineEffect where
  put (NewTweetUL_ x y) = put x >> put y
  get = do
    x <- get
    y <- get
    return $ NewTweetUL_ x y

--------------------------------------------------------------------------------
-- Timeline table : Key = UserID

data TimelineEffect = NewTweetTL_ UTCTime String

instance Serialize TimelineEffect where
  put (NewTweetTL_ x y) = put x >> put y
  get = do
    x <- get
    y <- get
    return $ NewTweetTL_ x y
