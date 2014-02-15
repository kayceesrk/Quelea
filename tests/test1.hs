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
  let test3 = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] (\_ -> prop true_) s
        r <- checkConsistency $ prop true_
        doIO $ print r
  runECD $(liftToSolverType ''Event) test3

  -----------------------------------------------------------------------------
  -- Test 4
  --
  -- Expect UnSat.
  putStrLn $ "Test 4: Expect Unsat"
  let test4 = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] (\_ -> prop true_) s
        b <- newAction "b" [| Deposit |] (\_ -> prop true_) s
        r <- checkConsistency $ prop true_
        doIO $ print r
  runECD $(liftToSolverType ''Event) test4

  -----------------------------------------------------------------------------
  -- Test 4.1
  --
  -- Expect Sat -> Asserting (not false).
  putStrLn $ "Test 4.1: Expect Sat"
  let test4_1 = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] (\_ -> prop true_) s
        b <- newAction "b" [| Deposit |] (\_ -> prop true_) s
        r <- checkConsistency $ prop false_
        doIO $ print r
  runECD $(liftToSolverType ''Event) test4_1

  -----------------------------------------------------------------------------
  -- Test 5
  --
  -- Expect Sat -> two actions performed on the same thread may not necessarily
  -- be visible to each other.
  putStrLn $ "Test 5: Expect Sat"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              (x `visTo` y) `or_` (y `visTo` x)
  let test5 = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] (\_ -> prop true_) s
        b <- newAction "b" [| Deposit |] (\_ -> prop true_) s
        r <- checkConsistency $ fol
        doIO $ print r
  runECD $(liftToSolverType ''Event) test5

  -----------------------------------------------------------------------------
  -- Test 6
  --
  -- Expect Unsat.
  putStrLn $ "Test 6: Expect Unsat"
  let fol = forall_ $ \ x -> forall_ $ \ y -> prop $
              ((inActSoup x) `and_` (inActSoup y)) `implies_`
              ((x `visTo` y) `or_` (y `visTo` x))
  let test6 = do
        s <- newSession "s1"
        a <- newAction "a" [| Deposit |] (\_ -> prop true_) s
        b <- newAction "b" [| Deposit |] readMyWrites s
        r <- checkConsistency $ fol
        doIO $ print r
  runECD $(liftToSolverType ''Event) test6
