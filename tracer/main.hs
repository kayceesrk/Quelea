{-# Language TemplateHaskell, ScopedTypeVariables #-}

import Tracer
import Language.Haskell.TH

data Event = Deposit | Withdraw

totalOrder :: FOL
totalOrder = forall_ $ \x -> forall_ $ \y -> prop $
  (not_ $ sameAct x y) `and_` (isEvent x [| Withdraw |]) `and_` (isEvent y [| Withdraw |])
  `implies_`
  ((x `visTo` y) `or_` (y `visTo` x))

totalOrderProp :: ExpQ -> FOL
totalOrderProp exp = forall_ $ \x -> forall_ $ \y -> prop $
  (not_ $ sameAct x y) `and_` (isEvent x exp) `and_` (isEvent y exp)
  `implies_`
  ((x `visTo` y) `or_` (y `visTo` x))

main = return ()
