{-# Language TemplateHaskell, ScopedTypeVariables #-}

import Tracer
import Language.Haskell.TH

data Event = Deposit | Withdraw deriving Show
instance SolverEvent Event

data Attr
instance Show Attr where
  show _ = ""

instance SolverAttr Attr


tabIO s = doIO $ (putStr "\t") >> s

main = do
  -----------------------------------------------------------------------------
  -- Test 1
  --
  -- Expect Sat
  putStrLn $ "Test 1: Expect Sat"
  let test = do
        r <- checkConsistency $ prop false_
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 2
  --
  -- Expect Unsat
  putStrLn $ "Test 2: Expect Unsat"
  let test = do
        r <- checkConsistency $ prop true_
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 3
  --
  -- Expect Unsat
  putStrLn $ "Test 3: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        r <- checkConsistency $ prop true_
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 3.1
  --
  -- Expect Unsat
  putStrLn $ "Test 3.1: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        r <- checkConsistency $ prop $ isInSameSess a a
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 3.2
  --
  -- Expect Unsat
  putStrLn $ "Test 3.2: Expect Unsat, Sat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        r <- checkConsistency $ prop $ isEvent a Deposit
        tabIO $ print r
        r <- checkConsistency $ prop $ isEvent a Withdraw
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test


  -----------------------------------------------------------------------------
  -- Test 4
  --
  -- Expect UnSat.
  putStrLn $ "Test 4: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        b <- newAction "b" Deposit basicEventual s
        r <- checkConsistency $ prop true_
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 4.1
  --
  -- Expect Sat -> Asserting (not false).
  putStrLn $ "Test 4.1: Expect Sat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        b <- newAction "b" Deposit basicEventual s
        r <- checkConsistency $ prop false_
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 4.2
  --
  -- Expect Unsat.
  putStrLn $ "Test 4.2: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        b <- newAction "b" Deposit basicEventual s
        r <- checkConsistency $ prop $ isInSameSess a b
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 4.3
  --
  -- Expect Unsat.
  putStrLn $ "Test 4.3: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        b <- newAction "b" Deposit basicEventual s
        r <- checkConsistency $ prop $ a `sessOrd` b
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 4.4
  --
  -- Expect Unsat.
  putStrLn $ "Test 4.4: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        b <- newAction "b" Deposit basicEventual s
        c <- newAction "c" Deposit basicEventual s
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
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test


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
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        b <- newAction "b" Deposit basicEventual s
        r <- checkConsistency $ fol
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 6
  --
  -- Expect Unsat.
  putStrLn $ "Test 6: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        b <- newAction "b" Deposit readMyWrites s
        r <- checkConsistency $ prop $ a `visTo` b
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

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
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        b <- newAction "b" Deposit readMyWrites s
        r <- checkConsistency $ fol
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 8
  --
  -- Expect Unsat.
  putStrLn $ "Test 8: Expect Unsat"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              (not_ $ sameAct x y) `and_`
               (inActSoup x) `and_` (inActSoup y) `and_`
               (isEvent x Withdraw) `and_` (isEvent y Withdraw)
              `implies_`
              ((x `visTo` y) `or_` (y `visTo` x))
  let test = do
        s <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s
        b <- newAction "b" Deposit readMyWrites s
        r <- checkConsistency $ fol
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 9
  --
  -- Expect Sat -> the withdraw operations are not totally ordered.
  putStrLn $ "Test 9: Expect Sat"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              (not_ $ sameAct x y) `and_`
               (inActSoup x) `and_` (inActSoup y) `and_`
               (isEvent x Withdraw) `and_` (isEvent y Withdraw)
              `implies_`
              ((x `visTo` y) `or_` (y `visTo` x))
  let test = do
        s1 <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s1
        b <- newAction "b" Withdraw readMyWrites s1
        s2 <- newSession "s2"
        c <- newAction "c" Deposit basicEventual s2
        d <- newAction "d" Withdraw readMyWrites s2
        r <- checkConsistency $ fol
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test

  -----------------------------------------------------------------------------
  -- Test 10
  --
  -- Expect Unsat.
  putStrLn $ "Test 10: Expect Unsat+"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              ((not_ $ sameAct x y) `and_`
               (inActSoup x) `and_` (inActSoup y) `and_`
               (isEvent x Withdraw) `and_` (isEvent y Withdraw))
              `implies_`
              ((x `visTo` y) `or_` (y `visTo` x))
  let test = do
        s1 <- newSession "s1"
        a <- newAction "a" Deposit basicEventual s1
        b <- newAction "b" Withdraw strong s1
        s2 <- newSession "s2"
        c <- newAction "c" Deposit basicEventual s2
        d <- newAction "d" Withdraw strong s2
        r <- checkConsistency $ prop $ isStrongAct b
        tabIO $ print r
        r <- checkConsistency $ prop $ isStrongAct d
        tabIO $ print r
        r <- checkConsistency $ prop $ (b `visTo` d) `or_` (d `visTo` b)
        tabIO $ print r
        r <- checkConsistency $ prop $ isEvent b Withdraw
        tabIO $ print r
        r <- checkConsistency $ prop $ isEvent d Withdraw
        tabIO $ print r
        r <- checkConsistency $ prop $ (isEvent a Deposit) `and_` (isEvent c Deposit)
        tabIO $ print r
        r <- checkConsistency fol
        tabIO $ print r
  runECD $(liftEvent ''Event) $(liftAttr ''Attr)  test
