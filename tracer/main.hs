{-# Language TemplateHaskell, ScopedTypeVariables #-}

import Tracer
import Language.Haskell.TH

data Event = Deposit | Withdraw

main = do
  let ecd = do {
    r <- checkConsistency $ prop false_;
    doIO $ print r }
  runECD $(liftToSolverType ''Event) ecd
