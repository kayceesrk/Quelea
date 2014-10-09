{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module MicroBlogDefs (
  UserID(..),
  UserEffect(..),
  Operation(..),
  createTables,
  dropTables,

  addUser, addUserCtrt,
  addUsername, addUsernameCtrt,
  getUserID, getUserIDCtrt
) where


import Database.Cassandra.CQL
import Data.Serialize as S
import Data.Time.Clock
import Data.UUID
import Control.Applicative ((<$>))

import Codeec.Types
import Codeec.Contract
import Codeec.TH
import Codeec.DBDriver

--------------------------------------------------------------------------------
-- User table : key = UserID

newtype UserID = UserID UUID deriving Eq

data UserEffect = AddUser_ String {- username -} String {- password -}
                | UpdateUser_ String String
                | AddFollowing_ UserID {- follows -}
                | AddFollower_ UserID {- followedBy -}

instance Serialize UserID where
  put (UserID uuid) = put uuid
  get = UserID <$> get

instance Serialize UserEffect where
  put (AddUser_ x y) = putWord8 0 >> put x >> put y
  put (AddFollowing_ x) = putWord8 1 >> put x
  put (AddFollower_ x) = putWord8 2 >> put x
  get = do
    i <- getWord8
    case i of
      0 -> do
        x <- get
        y <- get
        return $ AddUser_ x y
      1 -> AddFollowing_ <$> get
      2 -> AddFollower_ <$> get

instance CasType UserEffect where
  putCas = put
  getCas = get
  casType _ = CBlob


instance Effectish UserEffect where
  summarize l = l

addUser :: [UserEffect] -> (String, String) -> ((), Maybe UserEffect)
addUser _ (userName,password) = ((), Just $ AddUser_ userName password)

addFollower :: [UserEffect] -> UserID -> ((), Maybe UserEffect)
addFollower _ uid = ((), Just $ AddFollower_ uid)

addFollowing :: [UserEffect] -> UserID -> ((), Maybe UserEffect)
addFollowing _ uid = ((), Just $ AddFollowing_ uid)

getAddFollowers :: [UserEffect] -> () -> ([UserID], Maybe UserEffect)
getAddFollowers effs _ =
  let res = foldl (\acc e -> case e of
                               AddFollower_ uid -> uid:acc
                               otherwise -> acc) [] effs
  in (res, Nothing)

getAddFollowing :: [UserEffect] -> () -> ([UserID], Maybe UserEffect)
getAddFollowing effs _ =
  let res = foldl (\acc e -> case e of
                               AddFollowing_ uid -> uid:acc
                               otherwise -> acc) [] effs
  in (res, Nothing)

--------------------------------------------------------------------------------
-- Username table : Key = String

data UsernameEffect = AddUsername_ UserID
                    | GetUserID_
                        deriving Eq

instance Serialize UsernameEffect where
  put (AddUsername_ uid) = put uid
  get = AddUsername_ <$> get

instance Effectish UsernameEffect where
  summarize l = l

instance CasType UsernameEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

addUsername :: [UsernameEffect] -> UserID -> (Bool, Maybe UsernameEffect)
addUsername [] uid = (True, Just $ AddUsername_ uid)
addUsername _ _ = (False, Nothing)

getUserID :: [UsernameEffect] -> () -> (Maybe UserID, Maybe UsernameEffect)
getUserID [] _ = (Nothing, Nothing)
getUserID (AddUsername_ uid:_) _ = (Just uid, Nothing)

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

instance CasType TweetEffect where
  putCas = put
  getCas = get
  casType _ = CBlob


instance Effectish TweetEffect where
  summarize l = l

--------------------------------------------------------------------------------
-- UserLine table : Key = UserID

data UserlineEffect = NewTweetUL_ UTCTime String

instance Serialize UserlineEffect where
  put (NewTweetUL_ x y) = put x >> put y
  get = do
    x <- get
    y <- get
    return $ NewTweetUL_ x y

instance CasType UserlineEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

instance Effectish UserlineEffect where
  summarize l = l

--------------------------------------------------------------------------------
-- Timeline table : Key = UserID

data TimelineEffect = NewTweetTL_ UTCTime String

instance Serialize TimelineEffect where
  put (NewTweetTL_ x y) = put x >> put y
  get = do
    x <- get
    y <- get
    return $ NewTweetTL_ x y

instance CasType TimelineEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

instance Effectish TimelineEffect where
  summarize l = l

mkOperations [''UserEffect, ''UsernameEffect, ''TweetEffect, ''UserlineEffect, ''TimelineEffect]

--------------------------------------------------------------------------------
-- Contracts

trueCtrt :: Contract Operation
trueCtrt x = liftProp $ true

addUserCtrt :: Contract Operation
addUserCtrt = trueCtrt

getUserIDCtrt :: Contract Operation
getUserIDCtrt = trueCtrt

addUsernameCtrt :: Contract Operation
addUsernameCtrt a = forallQ_ [AddUsername] $ \b -> liftProp $ vis a b ∨ vis b a ∨ sameEff a b

--------------------------------------------------------------------------------

createTables :: Cas ()
createTables = do
  createTxnTable
  createTable "UserEffect"
  createTable "UsernameEffect"
  createTable "TweetEffect"
  createTable "TimelineEffect"
  createTable "UserlineEffect"

dropTables :: Cas ()
dropTables = do
  dropTxnTable
  dropTable "UserEffect"
  dropTable "UsernameEffect"
  dropTable "TweetEffect"
  dropTable "TimelineEffect"
  dropTable "UserlineEffect"
