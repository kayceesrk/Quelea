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
  -- Expect Unsat
  runTest $ prop false_

  -----------------------------------------------------------------------------
  -- Test 2
  --
  -- Expect Unsat
  runTest $ prop true_
