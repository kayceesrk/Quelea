{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, EmptyDataDecls #-}

module TestCtrtDefs (
  sc, cc, cv,
  rc, mav, psi, psiFlipped,
  Operation(..)
) where

import Quelea.Types
import Quelea.Contract
import Quelea.TH

data Dummy = Dummy1

mkOperations [''Dummy]

hbo :: OperationClass a => Effect -> Effect -> Prop a
hbo = AppRel $ TC $ ((So ∩ SameObj) ∪ Vis)

sc :: Fol ()
sc = forall2_ $ \a x ->  liftProp $ (hbo a x ∨ hbo x a ∨ AppRel SameEff a x) ∧
                                   (hbo a x ⇒ vis a x) ∧
                                   (hbo x a ⇒ vis x a)

cc :: Fol ()
cc = forall2_ $ \a x -> liftProp $ hbo a x ⇒ vis a x

cv :: Fol ()
cv = forall3_ $ \a b x -> liftProp $ (hbo a b ∧ vis b x) ⇒ vis a x

rc :: Fol ()
rc = forall3_ $ \a b c -> liftProp $ trans (SameTxn a b) (Single c) ∧ sameObjList [a,b,c] ∧ vis a c ⇒ vis b c

mav :: Fol ()
mav = forall4_ $ \a b c d -> liftProp $
        (trans (SameTxn a b) (DirDep c d) ∧ sameObj b c ∧ vis d a ∧ AppRel (So ∪ SameEff) a b ⇒ vis c b)

psi :: Fol ()
psi = forall4_ $ \a b c d -> liftProp $
        (trans (SameTxn a b) (DirDep c d) ∧ sameObj b d ∧ vis c a ⇒ vis d b)

psiFlipped :: Fol ()
psiFlipped = forall4_ $ \a b c d -> liftProp $
               trans (SameTxn c d) (SameTxn a b) ∧ sameObj b d ∧ vis c a ⇒ vis d b
