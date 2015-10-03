{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module MicroBlogDefs (
  UserID(..),
  TweetID(..),
  UserEffect(..),
  Operation(..),
  createTables,
  dropTables,

  addUser, addUserCtrt,
  addUsername, addUsernameCtrt,
  getUserID, getUserIDCtrt,
  getUserInfo, getUserInfoCtrt,

  addFollower, addFollowerCtrt,
  addFollowing, addFollowingCtrt,
  remFollower, remFollowerCtrt,
  remFollowing, remFollowingCtrt,
  getFollowers, getFollowersCtrt,
  getFollowing, getFollowingCtrt,

  addTweet, addTweetCtrt,
  getTweet, getTweetCtrt,

  addToUserline, addToUserlineCtrt,
  getTweetsInUserline, getTweetsInUserlineCtrt,
  addToTimeline, addToTimelineCtrt,
  getTweetsInTimeline, getTweetsInTimelineCtrt,

  addBlocks, addBlocksCtrt,
  addIsBlockedBy, addIsBlockedByCtrt,

  getBlocks, getBlocksCtrt,
  getIsBlockedBy, getIsBlockedByCtrt
) where


import Database.Cassandra.CQL
import Data.Serialize as S
import qualified Data.Map as M
import Data.Time.Clock
import Data.UUID
import Control.Applicative ((<$>))
import Data.Tuple.Select (sel1)
import Data.DeriveTH

import Quelea.Types
import Quelea.Contract
import Quelea.TH
import Quelea.DBDriver

--------------------------------------------------------------------------------
-- User table : key = UserID

newtype UserID = UserID UUID deriving (Eq, Ord)

data UserEffect = AddUser_ String {- username -} String {- password -}
                | AddFollowing_ UserID {- follows -} UTCTime
                | RemFollowing_ UserID UTCTime
                | AddFollower_ UserID {- followedBy -} UTCTime
                | RemFollower_ UserID UTCTime
                | Blocks_ UserID
                | GetBlocks_
                | IsBlockedBy_ UserID
                | GetIsBlockedBy_
                | GetUserInfo_
                | GetFollowers_
                | GetFollowing_ deriving Eq

$(derive makeSerialize ''UserID)

$(derive makeSerialize ''UserEffect)

instance CasType UserEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

instance Effectish UserEffect where
  summarize l = l

addUser :: [UserEffect] -> (String, String) -> ((), Maybe UserEffect)
addUser _ (userName,password) = ((), Just $ AddUser_ userName password)

getUserInfo :: [UserEffect] -> () -> (Maybe (String, String), Maybe UserEffect)
getUserInfo effs _ =
  let res = foldl (\acc eff ->
              case eff of
                AddUser_ userName password -> Just (userName, password)
                _ -> acc) Nothing effs
  in (res, Nothing)

isFollowing :: [UserEffect] -> UserID -> Bool
isFollowing effList targetUid =
  let foldedRes =
        foldl (\acc eff ->
          case (acc, eff) of
            (Nothing, x@(AddFollowing_ uid _)) | uid == targetUid -> Just x
            (Nothing, x@(RemFollowing_ uid _)) | uid == targetUid -> Just x
            (Just (AddFollowing_ uid1 ts1), x@(AddFollowing_ uid2 ts2)) | uid1 == uid2 && ts2 > ts1 -> Just x
            (Just (AddFollowing_ uid1 ts1), x@(RemFollowing_ uid2 ts2)) | uid1 == uid2 && ts2 > ts1 -> Just x
            (Just (RemFollowing_ uid1 ts1), x@(AddFollowing_ uid2 ts2)) | uid1 == uid2 && ts2 > ts1 -> Just x
            (Just (RemFollowing_ uid1 ts1), x@(RemFollowing_ uid2 ts2)) | uid1 == uid2 && ts2 > ts1 -> Just x
            otherwise -> acc) Nothing effList
  in case foldedRes of
       Nothing -> False
       Just (AddFollowing_ _ _) -> True
       Just (RemFollowing_ _ _) -> False
       otherwise -> error "isFollowing: unexpected value"

isFollowedBy :: [UserEffect] -> UserID -> Bool
isFollowedBy effList targetUid =
  let foldedRes =
        foldl (\acc eff ->
          case (acc, eff) of
            (Nothing, x@(AddFollower_ uid ts)) | uid == targetUid -> Just x
            (Nothing, x@(RemFollower_ uid ts)) | uid == targetUid -> Just x
            (Just (AddFollower_ uid1 ts1), x@(RemFollower_ uid2 ts2)) | uid1 == uid2 && ts2 > ts1 -> Just x
            (Just (RemFollower_ uid1 ts1), x@(AddFollower_ uid2 ts2)) | uid1 == uid2 && ts2 > ts1 -> Just x
            otherwise -> acc) Nothing effList
  in case foldedRes of
       Nothing -> False
       Just (AddFollower_ _ _) -> True
       Just (RemFollower_ _ _) -> False
       otherwise -> error "isFollowedBy: unexpected value"

addBlocks :: [UserEffect] -> UserID -> ((), Maybe UserEffect)
addBlocks _ uid = ((), Just $ Blocks_ uid)

addIsBlockedBy :: [UserEffect] -> UserID -> ((), Maybe UserEffect)
addIsBlockedBy _ uid = ((), Just $ IsBlockedBy_ uid)

getBlocks :: [UserEffect] -> UserID -> (Bool, Maybe UserEffect)
getBlocks effList targetUid = ((Blocks_ targetUid) `elem` effList, Nothing)

getIsBlockedBy :: [UserEffect] -> UserID -> (Bool, Maybe UserEffect)
getIsBlockedBy effList targetUid = ((IsBlockedBy_ targetUid) `elem` effList, Nothing)

addFollower :: [UserEffect] -> (UserID, UTCTime) -> (Bool, Maybe UserEffect)
addFollower effList (uid, timestamp) =
  if isFollowedBy effList uid
  then (True, Nothing)
  else if sel1 $ getBlocks effList uid
  then (False, Nothing)
  else (True, Just $ AddFollower_ uid timestamp)

remFollower :: [UserEffect] -> (UserID, UTCTime) -> ((), Maybe UserEffect)
remFollower _ (uid, timestamp) = ((), Just $ RemFollower_ uid timestamp)

remFollowing :: [UserEffect] -> (UserID, UTCTime) -> ((), Maybe UserEffect)
remFollowing _ (uid, timestamp) = ((), Just $ RemFollowing_ uid timestamp)


addFollowing :: [UserEffect] -> (UserID, UTCTime) -> (Bool, Maybe UserEffect)
addFollowing effList (uid, timestamp) =
  if isFollowing effList uid
  then (True, Nothing)
  else if sel1 $ getIsBlockedBy effList uid
  then (False, Nothing)
  else (True, Just $ AddFollowing_ uid timestamp)

data AddOrRem = Add | Rem
data ResolveState = Block | Other UTCTime AddOrRem

getFollowers :: [UserEffect] -> () -> ([UserID], Maybe UserEffect)
getFollowers effs _ =
  let resM = foldl (\m e ->
               case e of
                 AddFollower_ uid ts -> M.insertWith resolve uid (Other ts Add) m
                 RemFollower_ uid ts -> M.insertWith resolve uid (Other ts Rem) m
                 Blocks_ uid -> M.insert uid Block m) M.empty effs
      userList = M.foldlWithKey (\acc uid st ->
                    case st of
                      Other _ Add -> uid:acc
                      otherwise -> acc) [] resM
  in (userList, Nothing)
  where
    resolve x y =
      case (x,y) of
        (Other ts1 _, Other ts2 _) -> if ts2 > ts1 then y else x
        otherwise -> Block


getFollowing :: [UserEffect] -> () -> ([UserID], Maybe UserEffect)
getFollowing effs _ =
  let resM = foldl (\m e ->
               case e of
                 AddFollowing_ uid ts -> M.insertWith resolve uid (Other ts Add) m
                 RemFollowing_ uid ts -> M.insertWith resolve uid (Other ts Rem) m
                 IsBlockedBy_ uid -> M.insert uid Block m) M.empty effs
      userList = M.foldlWithKey (\acc uid st ->
                    case st of
                      Other _ Add -> uid:acc
                      otherwise -> acc) [] resM
  in (userList, Nothing)
  where
    resolve x y =
      case (x,y) of
        (Other ts1 _, Other ts2 _) -> if ts2 > ts1 then y else x
        otherwise -> Block
--------------------------------------------------------------------------------
-- Username table : Key = String

data UsernameEffect = AddUsername_ UserID
                    | GetUserID_
                        deriving Eq

$(derive makeSerialize ''UsernameEffect)

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
                 | GetTweet_

$(derive makeSerialize ''TweetID)

$(derive makeSerialize ''TweetEffect)

instance CasType TweetEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

instance Effectish TweetEffect where
  summarize l = l

addTweet :: [TweetEffect] -> (UserID, String, UTCTime) -> ((), Maybe TweetEffect)
addTweet _ (uid, tweet, time) = ((), Just $ NewTweet_ uid tweet time)

getTweet :: [TweetEffect] -> () -> (Maybe (UserID, String, UTCTime), Maybe TweetEffect)
getTweet [] _ = (Nothing, Nothing)
getTweet ((NewTweet_ x y z):_) _ = (Just $ (x,y,z), Nothing)

--------------------------------------------------------------------------------
-- UserLine table : Key = UserID

data UserlineEffect = NewTweetUL_ UTCTime TweetID
                    | GetTweetsInUL_

$(derive makeSerialize ''UserlineEffect)

instance CasType UserlineEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

instance Effectish UserlineEffect where
  summarize l = l

addToUserline :: [UserlineEffect] -> (UTCTime, TweetID) -> ((), Maybe UserlineEffect)
addToUserline _ (timestamp, tweetID) = ((), Just $ NewTweetUL_ timestamp tweetID)

getTweetsInUserline :: [UserlineEffect] -> () -> ([(UTCTime, TweetID)], Maybe UserlineEffect)
getTweetsInUserline effs _ =
  let res = foldl (\acc eff -> case eff of
                       NewTweetUL_ ts tid -> (ts,tid):acc
                       otherwise -> acc) [] effs
  in (res, Nothing)


--------------------------------------------------------------------------------
-- Timeline table : Key = UserID

data TimelineEffect = NewTweetTL_ UTCTime TweetID
                    | GetTweetsInTL_

$(derive makeSerialize ''TimelineEffect)

instance CasType TimelineEffect where
  putCas = put
  getCas = get
  casType _ = CBlob

instance Effectish TimelineEffect where
  summarize l = l

addToTimeline :: [TimelineEffect] -> (UTCTime, TweetID) -> ((), Maybe TimelineEffect)
addToTimeline _ (timestamp, tweetID) = ((), Just $ NewTweetTL_ timestamp tweetID)

getTweetsInTimeline :: [TimelineEffect] -> () -> ([(UTCTime, TweetID)], Maybe TimelineEffect)
getTweetsInTimeline effs _ =
  let res = foldl (\acc eff -> case eff of
                       NewTweetTL_ ts tid -> (ts,tid):acc
                       otherwise -> acc) [] effs
  in (res, Nothing)

--------------------------------------------------------------------------------

mkOperations [''UserEffect, ''UsernameEffect, ''TweetEffect, ''UserlineEffect, ''TimelineEffect]
$(derive makeSerialize ''Operation)

--------------------------------------------------------------------------------
-- Contracts

trueCtrt :: Contract Operation
trueCtrt x = liftProp $ true

addUserCtrt :: Contract Operation
addUserCtrt = trueCtrt

getUserInfoCtrt :: Contract Operation
getUserInfoCtrt x = forallQ_ [AddUser] $ \a -> liftProp $ soo a x ⇒ vis a x

addUsernameCtrt :: Contract Operation
addUsernameCtrt a = forallQ_ [AddUsername] $ \b -> liftProp $ vis a b ∨ vis b a ∨ sameEff a b

getUserIDCtrt :: Contract Operation
getUserIDCtrt x = forallQ_ [AddUsername] $ \a -> liftProp $ soo a x ⇒ vis a x

getFollowersCtrt :: Contract Operation
getFollowersCtrt x = forallQ_ [AddFollower] $ \a -> liftProp $ soo a x ⇒ vis a x

getFollowingCtrt :: Contract Operation
getFollowingCtrt x = forallQ_ [AddFollowing] $ \a -> liftProp $ soo a x ⇒ vis a x

addFollowerCtrt :: Contract Operation
addFollowerCtrt = trueCtrt

addFollowingCtrt :: Contract Operation
addFollowingCtrt = trueCtrt

remFollowingCtrt :: Contract Operation
remFollowingCtrt = trueCtrt

remFollowerCtrt :: Contract Operation
remFollowerCtrt = trueCtrt

getBlocksCtrt :: Contract Operation
getBlocksCtrt x = forallQ_ [Blocks] $ \a -> liftProp $ soo a x ⇒ vis a x

getIsBlockedByCtrt :: Contract Operation
getIsBlockedByCtrt x = forallQ_ [IsBlockedBy] $ \a -> liftProp $ soo a x ⇒ vis a x

addTweetCtrt :: Contract Operation
addTweetCtrt = trueCtrt

addToUserlineCtrt :: Contract Operation
addToUserlineCtrt = trueCtrt

addToTimelineCtrt :: Contract Operation
addToTimelineCtrt = trueCtrt

getTweetCtrt :: Contract Operation
getTweetCtrt x = forallQ2_ [NewTweet] [NewTweet] $ \a b -> liftProp $ soo a b ∧ vis b x ⇒ vis a x

getTweetsInTimelineCtrt :: Contract Operation
getTweetsInTimelineCtrt = trueCtrt

getTweetsInUserlineCtrt :: Contract Operation
getTweetsInUserlineCtrt = trueCtrt

addBlocksCtrt :: Contract Operation
addBlocksCtrt = trueCtrt

addIsBlockedByCtrt :: Contract Operation
addIsBlockedByCtrt = trueCtrt

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
