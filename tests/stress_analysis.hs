{-# LANGUAGE OverloadedStrings, DataKinds, TypeSynonymInstances, FlexibleInstances, BangPatterns, ScopedTypeVariables #-}

import Spec
import Control.Applicative
import Control.Monad
import System.Random
import qualified Data.Set as S
import Data.Array.IArray
import Data.Int (Int64)
import Data.UUID

cau :: Spec
cau x = forall_ $ \a -> hb a x ==> vis a x

main = do
  numInps::Int <- read <$> getLine
  inps::[(Int, Int64, [(Int,Int64)])] <- replicateM numInps $ read <$> getLine

  putStrLn "(declare-sort Effect)"
  putStrLn "(declare-sort Session)"

  let (maxSid,maxSidIdx,_) = last inps
  let numSess = maxSid + 1
  uuidList <- replicateM numSess $ randomIO
  let uuidArray :: Array Int UUID = listArray (0,maxSid) uuidList
  let context = map (\(sidInt, idx, addrIntList) ->
         let addrList = map (\(i,j) -> Addr (uuidArray!i) j) addrIntList
         in Z3Row (uuidArray!sidInt) idx $ S.fromList addrList) inps

  let curAct = Addr (uuidArray!maxSid) (maxSidIdx + 1)
  res <- isContextReady context curAct cau
  case res of
    NotReady addr-> putStrLn ("Not Ready. Wait for addr = " ++ show addr)
    Ready s -> putStrLn ("Ready : size(sub-known) = " ++ (show $ length s))
