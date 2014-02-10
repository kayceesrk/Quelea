{-# Language TemplateHaskell, ScopedTypeVariables #-}

import Tracer

data Event = Deposit | Withdraw

testProp :: FOL
testProp = Forall $ \ a -> Forall $ \ b -> Prop $
             (IsEvent a [| Withdraw |] `And`
              IsEvent b [| Withdraw |]) `Impl`
             ((a `VisTo` b) `Or` (b `VisTo` a))

main = do
  sort <- createEventSort ''Event
  return ()
