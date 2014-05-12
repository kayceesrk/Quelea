{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, GADTs, DeriveFunctor,
             TypeSynonymInstances, EmptyDataDecls #-}

module Spec
(
  Prop, Effect, Spec,

  -- Spec builders
  forall_, exists_, true, false, not_, (/\), (\/), (==>), sameEffect, vis, so,
  sortOf, ite, sameAttr, distinctEffects, isInSameSess,

  -- Queries
  isAvailable,
  isCoordFree,
  isWellTyped
) where

import Language.Haskell.TH
import Z3.Monad hiding (Result, showModel, Sort)
import qualified Z3.Monad as Z3M
import Control.Applicative hiding ((<*))
import Data.List (find)
import Control.Lens hiding (Effect)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Map as Map hiding (map)
import qualified Data.Set as Set
import Types

-------------------------------------------------------------------------------
-- Types

type Z3Sort = Z3M.Sort

data PropState = PropState {
  --------
  -- Sorts
  --------
  _effSort  :: Z3Sort,
  _sessSort :: Z3Sort,

  ------------
  -- Relations
  ------------
  _visRel  :: FuncDecl, -- Effect -> Effect -> Bool
  _soRel   :: FuncDecl, -- Effect -> Effect -> Bool
  {- Each effect has an address which is a pair composed of its session
   - identified (of sort Session) and an index (of soft Int) identifying its
   - position in the session. The following relations fetch session and index
   - of the effect. -}
  _sessRel :: FuncDecl, -- Effect -> Session
  _idxRel  :: FuncDecl  -- Effect -> Int
}

makeLenses ''PropState

newtype Prop = Prop { unProp :: ReaderT PropState Z3 AST }
newtype IntVal = IntVal { unIntVal :: ReaderT PropState Z3 AST }
newtype Effect = Effect { unEffect :: AST }

data Sort
class Attr a
type Spec = Effect -> Prop

-------------------------------------------------------------------------------
-- Helper

-- Monadic composition
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- f >=> g = \x -> f x >>= g

-------------------------------------------------------------------------------
-- Proposition builder

mkEffectConst :: ReaderT PropState Z3 Effect
mkEffectConst = do
  as <- view effSort
  lift $ Effect <$> mkFreshConst "Eff_" as

forall_ :: (Effect -> Prop) -> Prop
forall_ f = Prop $ do
  as <- view effSort
  qvConst <- lift $ mkFreshConst "FA_E_" as
  qv <- lift $ toApp qvConst
  body <- unProp $ f (Effect qvConst)
  lift $ mkForallConst [] [qv] body

-- Should not be exposed from the module
forallInt :: (IntVal -> Prop) -> Prop
forallInt f = Prop $ do
  is <- lift $ mkIntSort
  qvConst <- lift $ mkFreshConst "FA_I_" is
  qv <- lift $ toApp qvConst
  body <- (unProp . f . IntVal . lift . return) qvConst
  lift $ mkForallConst [] [qv] body

exists_ :: (Effect -> Prop) -> Prop
exists_ f = Prop $ do
  as <- view effSort
  qvConst <- lift $ mkFreshConst "EX_E_" as
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

(>*) :: IntVal -> IntVal -> Prop
(>*) (IntVal p1) (IntVal p2) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkGt ast1 ast2

(>=*) :: IntVal -> IntVal -> Prop
(>=*) (IntVal p1) (IntVal p2) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkGe ast1 ast2

(<*) :: IntVal -> IntVal -> Prop
(<*) (IntVal p1) (IntVal p2) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkLt ast1 ast2

(<=*) :: IntVal -> IntVal -> Prop
(<=*) (IntVal p1) (IntVal p2) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkLe ast1 ast2

(==*) :: IntVal -> IntVal -> Prop
(==*) (IntVal p1) (IntVal p2) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkEq ast1 ast2

(-*) :: IntVal -> IntVal -> IntVal
(-*) (IntVal p1) (IntVal p2) = IntVal $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkSub [ast1, ast2]

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
so (Effect a1) (Effect a2) = Prop $ do
  sr <- view soRel
  lift $ mkApp sr [a1, a2]

sortOf :: Effect -> Sort -> Prop
sortOf = undefined

idxOf :: Effect -> IntVal
idxOf (Effect a) = IntVal $ do
  ir <- view idxRel
  lift $ mkApp ir [a]

sameAttr :: Effect -> Effect -> Prop
sameAttr = undefined

isInSameSess :: Effect -> Effect -> Prop
isInSameSess (Effect a1) (Effect a2) = Prop $ do
  sr <- view sessRel
  as1 <- lift $ mkApp sr [a1]
  as2 <- lift $ mkApp sr [a2]
  lift $ mkEq as1 as2


ite :: Prop -> Prop -> Prop -> Prop
ite (Prop p1) (Prop p2) (Prop p3) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  ast3 <- p3
  lift $ mkIte ast1 ast2 ast3

-------------------------------------------------------------------------------
-- Queries

assertProp :: Prop -> ReaderT PropState Z3 ()
assertProp = unProp >=> (lift . assertCnstr)

assertBasicAxioms :: ReaderT PropState Z3 ()
assertBasicAxioms = do

  -- Visibility is asymmetric
  assertProp $ forall_ $ \ x -> forall_ $ \ y -> vis x y ==> (not_ $ vis y x)

  -- Session order is asymmetric
  assertProp $ forall_ $ \ x -> forall_ $ \ y -> so x y ==> (not_ $ so y x)

  -- Session order only relates actions from the same session. Otherwise, they
  -- are unrelated by session order.
  assertProp $ forall_ $ \ x -> forall_ $ \ y ->
    ite (isInSameSess x y /\ (not_ $ sameEffect x y))
    {- then -} (so x y \/ so y x)
    {- else -} (not_ $ so x y \/ so y x)

  -- Session order is transitive
  assertProp $ forall_ $ \ x -> forall_ $ \ y -> forall_ $ \ z ->
    (so x y /\ so y z) ==> (so x z)

  -- The index of an action is always >= 1
  let _1 = (IntVal . lift . mkInt) 1
  assertProp $ forall_ $ \ x -> forallInt $ \ i -> (i ==* idxOf x) ==> (i >=* _1)

  -- Existence of previous actions in the same session.
  -- For any action with a index greater than 1, there exists an action in the
  -- same session, with an index that is one less, and which precedes the
  -- original action in session order.
  assertProp $ forall_ $ \ a ->
    (idxOf a >* _1) ==> (exists_ $ \b -> (isInSameSess a b /\ (idxOf b ==* (idxOf a -* _1)) /\ so b a))

  -- If two actions are ordered by so, then their indices are ordered by <
  assertProp $ forall_ $ \a -> forall_ $ \b -> (so a b) ==> (idxOf a <* idxOf b)

  -- Two actions have the same address iff they are the same
  assertProp $ forall_ $ \a -> forall_ $ \b -> ((idxOf a ==* idxOf b) /\ isInSameSess a b) ==> sameEffect a b
  assertProp $ forall_ $ \a -> forall_ $ \b -> sameEffect a b ==> ((idxOf a ==* idxOf b) /\ isInSameSess a b)

  -- Instantiate a happens-before relation
  es <- view effSort
  hbFuncDecl <- lift $ do
    boolSort <- mkBoolSort
    mkFreshFuncDecl "hb" [es, es] boolSort
  let hb (Effect a1) (Effect a2) = Prop $ lift $ mkApp hbFuncDecl [a1,a2]

  -- Happens-before follows visibility and session order
  assertProp $ forall_ $ \a -> forall_ $ \b -> (vis a b \/ so a b) ==> hb a b
  -- Happens-before is transitive
  assertProp $ forall_ $ \ x -> forall_ $ \ y -> forall_ $ \ z ->
    (hb x y /\ hb y z) ==> (hb x z)
  -- Happens-before is acyclic
  assertProp $ forall_ $ \ x -> forall_ $ \ y -> hb x y ==> (not_ $ hb y x)


mkPropState :: Z3 PropState
mkPropState = do
  effectSort <- mkUninterpretedSort =<< mkStringSymbol "Effect"
  sessSort <- mkUninterpretedSort =<< mkStringSymbol "Session"
  boolSort <- mkBoolSort
  intSort <- mkIntSort

  visRel <- mkFreshFuncDecl "vis" [effectSort, effectSort] boolSort
  soRel <- mkFreshFuncDecl "so" [effectSort, effectSort] boolSort
  sessRel <- mkFreshFuncDecl "sess" [effectSort] sessSort
  idxRel <- mkFreshFuncDecl "idx" [effectSort] intSort

  return $ PropState effectSort sessSort visRel soRel sessRel idxRel

res2Bool :: Z3M.Result -> Bool
res2Bool Unsat = True
res2Bool Sat = False

-- http://rise4fun.com/Z3/v6jF
isAvailable :: Spec -> IO Bool
isAvailable s = evalZ3 $ do
  ps <- mkPropState
  runReaderT core ps
  where
    core = do
      -- assert basic axioms
      assertBasicAxioms

      -- Declare availability relation
      es <- view effSort
      avrFD <- lift $ do
        boolSort <- mkBoolSort
        mkFreshFuncDecl "avr" [es, es] boolSort
      let avr (Effect a) (Effect b) = Prop $ lift $ mkApp avrFD [a,b]

      -- Make an effect
      eff <- mkEffectConst

      -- Build availability relation and extend visibility
      assertProp $ forall_ $ \a -> forall_ $ \b -> vis a eff ==> avr a eff
      assertProp $ forall_ $ \a -> forall_ $ \b -> ((vis a b \/ so a b) /\ avr b eff) ==> (avr a eff)
      assertProp $ forall_ $ \a -> avr a eff ==> vis a eff

      -- Assert validity of the given specification under the extended context
      assertProp . not_ . s $ eff
      lift $ res2Bool <$> check

-- http://rise4fun.com/Z3/v6jF
isCoordFree :: Spec -> IO Bool
isCoordFree s = evalZ3 $ do
  ps <- mkPropState
  runReaderT core ps
  where
    core = do
      -- assert basic axioms
      assertBasicAxioms

      -- Declare a happens-before relation
      es <- view effSort
      hbFuncDecl <- lift $ do
        boolSort <- mkBoolSort
        mkFreshFuncDecl "hb" [es, es] boolSort
      let hb (Effect a1) (Effect a2) = Prop $ lift $ mkApp hbFuncDecl [a1,a2]

      -- Make an effect
      eff <- mkEffectConst

      -- Build happens-before relation and extend visibility (causal visibility)
      assertProp $ forall_ $ \a -> (vis a eff \/ so a eff) ==> hb a eff
      assertProp $ forall_ $ \a -> forall_ $ \b -> ((so a b \/ vis a b) /\ hb b eff) ==> hb a eff
      assertProp $ forall_ $ \a -> hb a eff ==> vis a eff

      -- Assert validity of the given specification under causal visibility
      assertProp . not_ . s $ eff
      lift $ res2Bool <$> check

isWellTyped :: Spec -> IO Bool
isWellTyped s = evalZ3 $ do
  ps <- mkPropState
  runReaderT core ps
  where
    core = do
      -- assert basic axioms
      assertBasicAxioms

      -- Declare a total order relation
      es <- view effSort
      toFuncDecl <- lift $ do
        boolSort <- mkBoolSort
        mkFreshFuncDecl "to" [es, es] boolSort
      let to (Effect a1) (Effect a2) = Prop $ lift $ mkApp toFuncDecl [a1,a2]

      -- Build the total order relation and extend visibility (sequential consistency)
      assertProp $ forall_ $ \a -> forall_ $ \b -> to a b \/ to b a \/ sameEffect a b
      assertProp $ forall_ $ \a -> forall_ $ \b -> so a b ==> to a b
      assertProp $ forall_ $ \a -> forall_ $ \b -> to a b ==> vis a b

      -- Assert the validity of the given specification under sequential consistency
      assertProp . not_ . forall_ $ s
      lift $ res2Bool <$> check
