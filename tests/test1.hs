{-# Language TemplateHaskell, ScopedTypeVariables #-}

import Tracer hiding (checkConsistency)
import qualified Tracer as T
import Language.Haskell.TH
import Control.Monad


data Event = Deposit | Withdraw deriving Show
instance SolverEvent Event

data Attr = Empty deriving Show
instance SolverAttr Attr


tabIO s = doIO $ (putStr "\t") >> s

checkConsistency fol testID expect = do
  r <- T.checkConsistency fol
  when (r /= expect) (tabIO $ putStrLn $ testID ++ " failed! (╯°□°)╯ ︵ ┻━┻ ")
  return r

main = do
  let emptyAttr :: [(Attr,String)] = []
  let sameObj a b = true_
  -----------------------------------------------------------------------------
  -- Test 1
  --
  -- Expect Fail
  putStrLn $ "Test 1: Expect Fail"
  let test = do
        r <- checkConsistency (prop false_) "1" Fail
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 2
  --
  -- Expect Ok
  putStrLn $ "Test 2: Expect Ok"
  let test = do
        r <- checkConsistency (prop true_) "2" Ok
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 3
  --
  -- Expect Ok
  putStrLn $ "Test 3: Expect Ok"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        r <- checkConsistency (prop true_) "3" Ok
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 3.1
  --
  -- Expect Ok
  putStrLn $ "Test 3.1: Expect Ok"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        r <- checkConsistency (prop $ isInSameSess a a) "3.1" Ok
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 3.2
  --
  -- Expect Ok
  putStrLn $ "Test 3.2: Expect Ok, Fail"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        r <- checkConsistency (prop $ isEventOf a Deposit) "3.2.1" Ok
        tabIO $ print r
        r <- checkConsistency (prop $ isEventOf a Withdraw) "3.2.2" Fail
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test


  -----------------------------------------------------------------------------
  -- Test 4
  --
  -- Expect UnFail.
  putStrLn $ "Test 4: Expect Ok"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        r <- checkConsistency (prop true_) "4" Ok
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 4.1
  --
  -- Expect Fail -> Asserting (not false).
  putStrLn $ "Test 4.1: Expect Fail"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        r <- checkConsistency (prop false_) "4.1" Fail
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 4.2
  --
  -- Expect Ok.
  putStrLn $ "Test 4.2: Expect Ok"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        r <- checkConsistency (prop $ isInSameSess a b) "4.2" Ok
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 4.3
  --
  -- Expect Ok.
  putStrLn $ "Test 4.3: Expect Ok"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        r <- checkConsistency (prop $ a `sessOrd` b) "4.3" Ok
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 4.4
  --
  -- Expect Ok.
  putStrLn $ "Test 4.4: Expect Ok"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        c <- addAction "c" Deposit emptyAttr basicEventual s
        r <- checkConsistency (prop $ isInSameSess a b) "4.4.1" Ok
        tabIO $ print r
        r <- checkConsistency (prop $ isInSameSess a c) "4.4.2" Ok
        tabIO $ print r
        r <- checkConsistency (prop $ isInSameSess b c) "4.4.3" Ok
        tabIO $ print r
        r <- checkConsistency (prop $ a `sessOrd` b) "4.4.4" Ok
        tabIO $ print r
        r <- checkConsistency (prop $ a `sessOrd` c) "4.4.5" Ok
        tabIO $ print r
        r <- checkConsistency (prop $ b `sessOrd` c) "4.4.6" Ok
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test


  -----------------------------------------------------------------------------
  -- Test 5
  --
  -- Expect Fail -> two actions performed on the same thread may not necessarily
  -- be visible to each other.
  putStrLn $ "Test 5: Expect Fail"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              ((not_ $ sameAct x y) `and_` (inActSoup x) `and_` (inActSoup y))
              `implies_`
              ((x `visTo` y) `or_` (y `visTo` x))
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        r <- checkConsistency fol "5" Fail
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 6
  --
  -- Expect Ok.
  putStrLn $ "Test 6: Expect Ok"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr readMyWrites s
        r <- checkConsistency (prop $ a `visTo` b) "6" Ok
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 7
  --
  -- Expect Ok.
  putStrLn $ "Test 7: Expect Ok"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              ((not_ $ sameAct x y) `and_` (inActSoup x) `and_` (inActSoup y))
              `implies_`
              ((x `visTo` y) `or_` (y `visTo` x))
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr readMyWrites s
        r <- checkConsistency fol "7" Ok
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 8
  --
  -- Expect Ok.
  putStrLn $ "Test 8: Expect Ok"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              (not_ $ sameAct x y) `and_`
               (inActSoup x) `and_` (inActSoup y) `and_`
               (isEventOf x Withdraw) `and_` (isEventOf y Withdraw)
              `implies_`
              ((x `visTo` y) `or_` (y `visTo` x))
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr readMyWrites s
        r <- checkConsistency fol "8" Ok
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 9
  --
  -- Expect Fail -> the withdraw operations are not totally ordered.
  putStrLn $ "Test 9: Expect Fail"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              (not_ $ sameAct x y) `and_`
               (inActSoup x) `and_` (inActSoup y) `and_`
               (isEventOf x Withdraw) `and_` (isEventOf y Withdraw)
              `implies_`
              ((x `visTo` y) `or_` (y `visTo` x))
  let test = do
        s1 <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s1
        b <- addAction "b" Withdraw emptyAttr readMyWrites s1
        s2 <- addSession "s2"
        c <- addAction "c" Deposit emptyAttr basicEventual s2
        d <- addAction "d" Withdraw emptyAttr readMyWrites s2
        r <- checkConsistency fol "9" Fail
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 10
  --
  -- Expect Ok.
  putStrLn $ "Test 10: Expect Ok+"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              ((not_ $ sameAct x y) `and_`
               (inActSoup x) `and_` (inActSoup y) `and_`
               (isEventOf x Withdraw) `and_` (isEventOf y Withdraw))
              `implies_`
              ((x `visTo` y) `or_` (y `visTo` x))
  let test = do
        s1 <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s1
        b <- addAction "b" Withdraw emptyAttr strong s1
        s2 <- addSession "s2"
        c <- addAction "c" Deposit emptyAttr basicEventual s2
        d <- addAction "d" Withdraw emptyAttr strong s2
        r <- checkConsistency (prop $ isStrongAct b) "10.1" Ok
        tabIO $ print r
        r <- checkConsistency (prop $ isStrongAct d) "10.2" Ok
        tabIO $ print r
        r <- checkConsistency (prop $ (b `visTo` d) `or_` (d `visTo` b)) "10.3" Ok
        tabIO $ print r
        r <- checkConsistency (prop $ isEventOf b Withdraw) "10.4" Ok
        tabIO $ print r
        r <- checkConsistency (prop $ isEventOf d Withdraw) "10.5" Ok
        tabIO $ print r
        r <- checkConsistency (prop $ (isEventOf a Deposit) `and_` (isEventOf c Deposit)) "10.6" Ok
        tabIO $ print r
        r <- checkConsistency fol "10.7" Ok
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test
