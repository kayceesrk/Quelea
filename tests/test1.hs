{-# Language TemplateHaskell, ScopedTypeVariables #-}

import Tracer
import Language.Haskell.TH

data Event = Deposit | Withdraw deriving Show
instance SolverEvent Event

data Attr = Empty deriving Show
instance SolverAttr Attr


tabIO s = doIO $ (putStr "\t") >> s

main = do
  let emptyAttr :: [(Attr,String)] = []
  let sameObj a b = true_
  -----------------------------------------------------------------------------
  -- Test 1
  --
  -- Expect Sat
  putStrLn $ "Test 1: Expect Sat"
  let test = do
        r <- checkConsistency $ prop false_
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 2
  --
  -- Expect Unsat
  putStrLn $ "Test 2: Expect Unsat"
  let test = do
        r <- checkConsistency $ prop true_
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 3
  --
  -- Expect Unsat
  putStrLn $ "Test 3: Expect Unsat"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        r <- checkConsistency $ prop true_
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 3.1
  --
  -- Expect Unsat
  putStrLn $ "Test 3.1: Expect Unsat"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        r <- checkConsistency $ prop $ isInSameSess a a
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 3.2
  --
  -- Expect Unsat
  putStrLn $ "Test 3.2: Expect Unsat, Sat"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        r <- checkConsistency $ prop $ isEventOf a Deposit
        tabIO $ print r
        r <- checkConsistency $ prop $ isEventOf a Withdraw
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test


  -----------------------------------------------------------------------------
  -- Test 4
  --
  -- Expect UnSat.
  putStrLn $ "Test 4: Expect Unsat"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        r <- checkConsistency $ prop true_
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 4.1
  --
  -- Expect Sat -> Asserting (not false).
  putStrLn $ "Test 4.1: Expect Sat"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        r <- checkConsistency $ prop false_
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 4.2
  --
  -- Expect Unsat.
  putStrLn $ "Test 4.2: Expect Unsat"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        r <- checkConsistency $ prop $ isInSameSess a b
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 4.3
  --
  -- Expect Unsat.
  putStrLn $ "Test 4.3: Expect Unsat"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        r <- checkConsistency $ prop $ a `sessOrd` b
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 4.4
  --
  -- Expect Unsat.
  putStrLn $ "Test 4.4: Expect Unsat"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        c <- addAction "c" Deposit emptyAttr basicEventual s
        r <- checkConsistency $ prop $ isInSameSess a b
        tabIO $ print r
        r <- checkConsistency $ prop $ isInSameSess a c
        tabIO $ print r
        r <- checkConsistency $ prop $ isInSameSess b c
        tabIO $ print r
        r <- checkConsistency $ prop $ a `sessOrd` b
        tabIO $ print r
        r <- checkConsistency $ prop $ a `sessOrd` c
        tabIO $ print r
        r <- checkConsistency $ prop $ b `sessOrd` c
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test


  -----------------------------------------------------------------------------
  -- Test 5
  --
  -- Expect Sat -> two actions performed on the same thread may not necessarily
  -- be visible to each other.
  putStrLn $ "Test 5: Expect Sat"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              ((not_ $ sameAct x y) `and_` (inActSoup x) `and_` (inActSoup y))
              `implies_`
              ((x `visTo` y) `or_` (y `visTo` x))
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr basicEventual s
        r <- checkConsistency $ fol
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 6
  --
  -- Expect Unsat.
  putStrLn $ "Test 6: Expect Unsat"
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr readMyWrites s
        r <- checkConsistency $ prop $ a `visTo` b
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 7
  --
  -- Expect Unsat.
  putStrLn $ "Test 7: Expect Unsat"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              ((not_ $ sameAct x y) `and_` (inActSoup x) `and_` (inActSoup y))
              `implies_`
              ((x `visTo` y) `or_` (y `visTo` x))
  let test = do
        s <- addSession "s1"
        a <- addAction "a" Deposit emptyAttr basicEventual s
        b <- addAction "b" Deposit emptyAttr readMyWrites s
        r <- checkConsistency $ fol
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 8
  --
  -- Expect Unsat.
  putStrLn $ "Test 8: Expect Unsat"
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
        r <- checkConsistency $ fol
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 9
  --
  -- Expect Sat -> the withdraw operations are not totally ordered.
  putStrLn $ "Test 9: Expect Sat"
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
        r <- checkConsistency $ fol
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test

  -----------------------------------------------------------------------------
  -- Test 10
  --
  -- Expect Unsat.
  putStrLn $ "Test 10: Expect Unsat+"
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
        r <- checkConsistency $ prop $ isStrongAct b
        tabIO $ print r
        r <- checkConsistency $ prop $ isStrongAct d
        tabIO $ print r
        r <- checkConsistency $ prop $ (b `visTo` d) `or_` (d `visTo` b)
        tabIO $ print r
        r <- checkConsistency $ prop $ isEventOf b Withdraw
        tabIO $ print r
        r <- checkConsistency $ prop $ isEventOf d Withdraw
        tabIO $ print r
        r <- checkConsistency $ prop $ (isEventOf a Deposit) `and_` (isEventOf c Deposit)
        tabIO $ print r
        r <- checkConsistency fol
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr) sameObj test
