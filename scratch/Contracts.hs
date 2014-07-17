{-# Language TemplateHaskell #-}

module Contracts (
  simple,
  rmw,
  tv,
  tv2,
  mw,
  lastEff,
  tv3,
  cyclic,

  OpName
) where

import Codeec.Contract
import Codeec.TH

data X = X_

mkOperations [''X]


cyclic :: Contract ()
cyclic x = liftProp $ appRel ((^+) (So ∪ Vis)) x x

simple :: Contract ()
simple x = liftProp $ true

rmw :: Contract ()
rmw a = forall_ $ \b -> liftProp $ soo b a ⇒ vis b a

tv :: Contract ()
tv z = forall_ $ \x -> forall_ $ \y -> liftProp $ vis x y ∧ vis y z ⇒ vis x z

tv2 :: Contract ()
tv2 x = forall_ $ \y -> forall_ $ \z -> liftProp $ vis x y ∧ vis y z ⇒ vis x z

mw :: Contract ()
mw x = forall_ $ \a -> forall_ $ \b -> liftProp $ soo a b ∧ vis b x ⇒ vis a x

lastEff :: Contract ()
lastEff x = forall_ $ \a -> liftProp $ vis a x ∨ sameeff a x

tv3 :: Contract ()
tv3 x = forall_ $ \a -> liftProp $ appRel Sameobj a x ⇒ (vis a x ∨ vis x a ∨ sameeff a x)
