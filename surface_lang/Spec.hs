{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, GADTs, DeriveFunctor,
             TypeSynonymInstances, EmptyDataDecls #-}

module Spec
(
  Prop, Effect, Spec,

  -- Spec builders
  forall_, exists_, true, false, not_, (/\), (\/), (==>), sameEffect, vis, so,
  sortOf, ite, sameAttr, distinctEffects, isInSameSess,
) where

import Language.Haskell.TH
import Z3.Monad hiding (Result, showModel, Sort)
import qualified Z3.Monad as Z3M
import Control.Applicative
import Data.List (find)
import Control.Lens hiding (Effect)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Map as Map hiding (map)
import qualified Data.Set as Set

type Z3Sort = Z3M.Sort

data PropState = PropState {
                   -- Sorts
                   _actSort :: Z3Sort,

                   -- Relations
                   _visRel :: FuncDecl -- Effect -> Effect -> Bool
                 }

makeLenses ''PropState

newtype Prop = Prop { unProp :: ReaderT PropState Z3 AST }
newtype Effect = Effect { unEffect :: AST }

data Sort
class Attr a
type Spec = Effect -> Prop

forall_ :: (Effect -> Prop) -> Prop
forall_ f = Prop $ do
  as <- view actSort
  qvConst <- lift $ mkFreshConst "FA_" as
  qv <- lift $ toApp qvConst
  body <- unProp $ f (Effect qvConst)
  lift $ mkForallConst [] [qv] body

exists_ :: (Effect -> Prop) -> Prop
exists_ f = Prop $ do
  as <- view actSort
  qvConst <- lift $ mkFreshConst "EX_" as
  qv <- lift $ toApp qvConst
  body <- unProp $ f (Effect qvConst)
  lift $ mkExistsConst [] [qv] body

true :: Prop
true = Prop $ lift mkTrue

false :: Prop
false = Prop $ lift mkFalse

not_ :: Prop -> Prop
not_ (Prop p) = Prop $ do
  ast <- p
  lift $ mkNot ast

(\/) :: Prop -> Prop -> Prop
(\/) (Prop p1) (Prop p2) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkOr [ast1, ast2]

(/\) :: Prop -> Prop -> Prop
(/\) (Prop p1) (Prop p2) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkAnd [ast1, ast2]

(==>) :: Prop -> Prop -> Prop
(==>) (Prop p1) (Prop p2) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkImplies ast1 ast2

sameEffect :: Effect -> Effect -> Prop
sameEffect (Effect a1) (Effect a2) =
  Prop $ lift $ mkEq a1 a2

distinctEffects :: [Effect] -> Prop
distinctEffects al =
  Prop $ lift $ mkDistinct $ map unEffect al

vis :: Effect -> Effect -> Prop
vis (Effect a1) (Effect a2) = Prop $ do
  vr <- view visRel
  lift $ mkApp vr [a1,a2]

so :: Effect -> Effect -> Prop
so = undefined

sortOf :: Effect -> Sort -> Prop
sortOf = undefined

sameAttr :: Effect -> Effect -> Prop
sameAttr = undefined

isInSameSess :: Effect -> Effect -> Prop
isInSameSess = undefined

ite :: Prop -> Prop -> Prop -> Prop
ite = undefined
