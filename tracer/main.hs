{-# Language TemplateHaskell, ScopedTypeVariables #-}

import Tracer

data Event = Deposit | Withdraw

main = do
  sort <- createZ3EventType ''Event
  return ()
