{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Codeec.Contract.Language (
  Rel(..),
  Prop(..),
  Fol(..),
  Contract,
  Effect(..),
  Z3CtrtState(..),
  Z3Ctrt(..),

  true,
  vis,
  so,
  soo,
  hb,
  sameeff,
  (∩),
  (∪),
  (∧),
  (∨),
  (⇒),
  (^+),
  appRel,
  liftProp,
  forall_,
  forallQ_

) where


import Codeec.Types
import Z3.Monad
import qualified Data.Map as M
import Control.Monad.State

-------------------------------------------------------------------------------
-- Types

data Z3CtrtState = Z3CtrtState {
  -- Sorts
  _effSort  :: Sort,
  _operSort :: Sort,
  -- Relations
  _visRel     :: FuncDecl, -- Effect -> Effect -> Bool
  _soRel      :: FuncDecl, -- Effect -> Effect -> Bool
  _sameobjRel :: FuncDecl, -- Effect -> Effect -> Bool
  _operRel    :: FuncDecl, -- Effect -> Oper
  -- Map
  _effMap :: M.Map Effect AST,
  -- Assertions
  _assertions :: [AST]
}

data Z3Ctrt = Z3Ctrt { unZ3Ctrt :: StateT Z3CtrtState Z3 AST }

newtype Effect = Effect { unEffect :: Int } deriving (Eq, Ord)

data Rel = Vis | So | Sameobj | TC Rel | Sameeff
         | Union Rel Rel | Intersect Rel Rel
data Operation a => Prop a =
    PTrue | Not (Prop a) | AppRel Rel Effect Effect | Conj (Prop a) (Prop a)
  | Disj (Prop a) (Prop a) | Impl (Prop a) (Prop a) | Oper Effect a | Raw Z3Ctrt
data Operation a => Fol a = Forall [a] (Effect -> Fol a) | Plain (Prop a)

type Contract a = Effect -> Fol a

-------------------------------------------------------------------------------
-- Contract builder

true :: Prop a
true = PTrue

vis :: Effect -> Effect -> Prop a
vis a b = AppRel Vis a b

so :: Effect -> Effect -> Prop a
so a b = AppRel So a b

soo :: Effect -> Effect -> Prop a
soo a b = AppRel (So ∩ Sameobj) a b

hb :: Effect -> Effect -> Prop a
hb a b = AppRel (TC $ Union Vis So) a b

(∪) :: Rel -> Rel -> Rel
a ∪ b = Union a b

(∩) :: Rel -> Rel -> Rel
a ∩ b = Intersect a b

(∧) :: Operation a => Prop a -> Prop a -> Prop a
(∧) = Conj

(∨) :: Operation a => Prop a -> Prop a -> Prop a
(∨) = Disj

(⇒) :: Operation a => Prop a -> Prop a -> Prop a
(⇒) = Impl

(^+) :: Rel -> Rel
(^+) = TC

appRel :: Operation a => Rel -> Effect -> Effect -> Prop a
appRel = AppRel

liftProp :: Operation a => Prop a -> Fol a
liftProp = Plain

forall_ :: Operation a => (Effect -> Fol a) -> Fol a
forall_ f = Forall [] f

forallQ_ :: Operation a => [a] -> (Effect -> Fol a) -> Fol a
forallQ_ q f = Forall q f

sameeff :: Effect -> Effect -> Prop a
sameeff a b = AppRel Sameeff a b
