{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, EmptyDataDecls #-}

module TestCtrtDefs (
  sc, cc, cv,
  rc, mav, psi, psiFlipped,
  Operation(..)
) where

import Codeec.Types
import Codeec.Contract
import Codeec.TH

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
rc = forall3_ $ \a b c -> liftProp $ trans[[a,b],[c]] ∧ sameObjList [a,b,c] ∧ vis a c ⇒ vis b c

mav :: Fol ()
mav = forall4_ $ \a b c d -> forall3_ $ \e f g -> liftProp $
        (trans[[a,b],[c,d]] ∧ sameObj b d ∧ vis c a ∧ AppRel (So ∪ SameEff) a b ⇒ vis d b) ∧
        (trans[[e,f],[g]] ∧ sameObjList [e,f,g] ∧ vis e g ⇒ vis f g)

psi :: Fol ()
psi = forall4_ $ \a b c d -> forall3_ $ \e f g -> liftProp $
        (trans[[a,b],[c,d]] ∧ sameObj b d ∧ vis c a ⇒ vis d b) ∧
        (trans[[e,f],[g]] ∧ sameObjList [e,f,g] ∧ vis e g ⇒ vis f g)

psiFlipped :: Fol ()
psiFlipped = forall4_ $ \a b c d -> liftProp $ (trans[[c,d],[a,b]] ∧ sameObj b d ∧ vis c a ⇒ vis d b)
