{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Codeec.Contract.Language (
  Rel(..),
  Prop(..),
  Fol,
  Contract,
  Effect,

  true_,
  vis,
  so,
  soo,
  hb,
  (∩),
  (∪),
  liftProp,
  forall_,
  forallQ_

) where


import Codeec.Types

-------------------------------------------------------------------------------
-- Types

newtype Effect = Effect { unEffect :: Int }

data Rel = Vis | So | Sameobj | TC Rel
         | Union Rel Rel | Intersect Rel Rel
data Prop = PTrue | AppRel Rel Effect Effect | Conj Prop Prop
          | Disj Prop Prop | Impl Prop Prop
data OperName a => Fol a = Forall [a] (Effect -> Fol a) | Plain Prop
type Contract a = Effect -> Fol a

-------------------------------------------------------------------------------
-- Contract builder

true_ :: Prop
true_ = PTrue

vis :: Effect -> Effect -> Prop
vis a b = AppRel Vis a b

so :: Effect -> Effect -> Prop
so a b = AppRel So a b

soo :: Effect -> Effect -> Prop
soo a b = AppRel (So ∩ Sameobj) a b

hb :: Effect -> Effect -> Prop
hb a b = AppRel (TC $ Union Vis So) a b

(∪) :: Rel -> Rel -> Rel
a ∪ b = Union a b

(∩) :: Rel -> Rel -> Rel
a ∩ b = Intersect a b

liftProp :: Prop -> Fol a
liftProp p = Plain p

forall_ :: OperName a => (Effect -> Fol a) -> Fol a
forall_ f = Forall [] f

forallQ_ :: OperName a => [a] -> (Effect -> Fol a) -> Fol a
forallQ_ q f = Forall q f
