{-# Language TemplateHaskell, ScopedTypeVariables #-}

import Tracer
import Language.Haskell.TH

data Event = Deposit | Withdraw

checkFol fol = do
  r <- checkConsistency fol
  doIO $ print r

runTest fol = runECD $(liftToSolverType ''Event) $ checkFol fol

main = do
  -----------------------------------------------------------------------------
  -- Test 1
  --
  -- Expect Sat
  putStrLn $ "Test 1: Expect Sat"
  runTest $ prop false_

  -----------------------------------------------------------------------------
  -- Test 2
  --
  -- Expect Unsat
  putStrLn $ "Test 2: Expect Unsat"
  runTest $ prop true_

  -----------------------------------------------------------------------------
  -- Test 3
  --
  -- Expect Unsat
  putStrLn $ "Test 3: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] basicEventual s
        r <- checkConsistency $ prop true_
        doIO $ print r
  runECD $(liftToSolverType ''Event) test

  -----------------------------------------------------------------------------
  -- Test 3.1
  --
  -- Expect Unsat
  putStrLn $ "Test 3.1: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] basicEventual s
        r <- checkConsistency $ prop $ isInSameSess a a
        doIO $ print r
  runECD $(liftToSolverType ''Event) test

  -----------------------------------------------------------------------------
  -- Test 4
  --
  -- Expect UnSat.
  putStrLn $ "Test 4: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] basicEventual s
        b <- newAction "b" [| Deposit |] basicEventual s
        r <- checkConsistency $ prop true_
        doIO $ print r
  runECD $(liftToSolverType ''Event) test

  -----------------------------------------------------------------------------
  -- Test 4.1
  --
  -- Expect Sat -> Asserting (not false).
  putStrLn $ "Test 4.1: Expect Sat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] basicEventual s
        b <- newAction "b" [| Deposit |] basicEventual s
        r <- checkConsistency $ prop false_
        doIO $ print r
  runECD $(liftToSolverType ''Event) test

  -----------------------------------------------------------------------------
  -- Test 4.2
  --
  -- Expect Unsat.
  putStrLn $ "Test 4.2: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] basicEventual s
        b <- newAction "b" [| Deposit |] basicEventual s
        r <- checkConsistency $ prop $ isInSameSess a b
        doIO $ print r
  runECD $(liftToSolverType ''Event) test

  -----------------------------------------------------------------------------
  -- Test 4.3
  --
  -- Expect Unsat.
  putStrLn $ "Test 4.3: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] basicEventual s
        b <- newAction "b" [| Deposit |] basicEventual s
        r <- checkConsistency $ prop $ a `sessOrd` b
        doIO $ print r
  runECD $(liftToSolverType ''Event) test

  -----------------------------------------------------------------------------
  -- Test 4.4
  --
  -- Expect Unsat.
  putStrLn $ "Test 4.4: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] basicEventual s
        b <- newAction "b" [| Deposit |] basicEventual s
        c <- newAction "c" [| Deposit |] basicEventual s
        r <- checkConsistency $ prop $ isInSameSess a b
        doIO $ print r
        r <- checkConsistency $ prop $ isInSameSess a c
        doIO $ print r
        r <- checkConsistency $ prop $ isInSameSess b c
        doIO $ print r
        r <- checkConsistency $ prop $ a `sessOrd` b
        doIO $ print r
        r <- checkConsistency $ prop $ a `sessOrd` c
        doIO $ print r
        r <- checkConsistency $ prop $ b `sessOrd` c
        doIO $ print r
  runECD $(liftToSolverType ''Event) test


  -----------------------------------------------------------------------------
  -- Test 5
  --
  -- Expect Sat -> two actions performed on the same thread may not necessarily
  -- be visible to each other.
  putStrLn $ "Test 5: Expect Sat"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              ((not_ $ sameAct x y) `and_` (inActSoup x) `and_` (inActSoup y))
              `implies_`
              (x `visTo` y) `or_` (y `visTo` x)
  let test = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] basicEventual s
        b <- newAction "b" [| Deposit |] basicEventual s
        r <- checkConsistency $ fol
        doIO $ print r
  runECD $(liftToSolverType ''Event) test

  -----------------------------------------------------------------------------
  -- Test 6
  --
  -- Expect Unsat.
  putStrLn $ "Test 6: Expect Unsat"
  let test = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] basicEventual s
        b <- newAction "b" [| Deposit |] readMyWrites s
        r <- checkConsistency $ prop $ a `visTo` b
        doIO $ print r
  runECD $(liftToSolverType ''Event) test

  -----------------------------------------------------------------------------
  -- Test 7
  --
  -- Expect Unsat.
  putStrLn $ "Test 7: Expect Unsat"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              ((not_ $ sameAct x y) `and_` (inActSoup x) `and_` (inActSoup y))
              `implies_`
              (x `visTo` y) `or_` (y `visTo` x)
  let test = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] basicEventual s
        b <- newAction "b" [| Deposit |] readMyWrites s
        r <- checkConsistency $ fol
        doIO $ print r
  runECD $(liftToSolverType ''Event) test
