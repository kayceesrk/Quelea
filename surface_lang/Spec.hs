{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, GADTs, DeriveFunctor, RankNTypes,
             TypeSynonymInstances, EmptyDataDecls, DoAndIfThenElse #-}

module Spec
(
  Prop, Effect, Spec,

  -- Spec builders
  forall_, exists_, true, false, not_, (/\), (\/), (==>), sameEffect, vis, so,
  sortOf, ite, sameAttr, distinctEffects, isInSameSess,

  -- Queries
  isAvailable,
  isCoordFree,
  isWellTyped,
  isContextReady
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
import qualified Data.Map as M
import qualified Data.Set as S
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
  -- Visibility relation
  _visRel  :: FuncDecl, -- Effect -> Effect -> Bool
  -- Session order relation
  _soRel   :: FuncDecl, -- Effect -> Effect -> Bool
  -- Happens-before relation
  _hbRel   :: FuncDecl, -- Effect -> Effect -> Bool
  {- Each effect has an address which is a pair composed of its session
   - identified (of sort Session) and an index (of soft Int) identifying its
   - position in the session. The following relations fetch session and index
   - of the effect. -}
  _sessRel :: FuncDecl, -- Effect -> Session
  _idxRel  :: FuncDecl  -- Effect -> Int
}

makeLenses ''PropState

newtype Effect = Effect { unEffect :: AST }
newtype Session = Session { unSession :: AST }

-- State used for runtime Z3 checks
data RtState = RtState {
  _propState  :: PropState,
  _effMap     :: M.Map Addr Effect, -- Maps addr to Z3 effect const
  _sessMap    :: M.Map Sess Session, -- Maps sess to Z3 effect const
  _knownSet   :: FuncDecl, -- Effect -> Bool
  _unknownSet :: FuncDecl  -- Effect -> Bool
}

makeLenses ''RtState

newtype Prop = Prop { unProp :: ReaderT PropState Z3 AST }
newtype IntVal = IntVal { unIntVal :: ReaderT PropState Z3 AST }


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

orList :: [Prop] -> Prop
orList propList = Prop $ do
  astList <- sequence $ unProp <$> propList
  lift $ mkOr astList

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

hb :: Effect -> Effect -> Prop
hb (Effect a1) (Effect a2) = Prop $ do
  hr <- view hbRel
  lift $ mkApp hr [a1, a2]

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

hasSess :: Effect -> Session -> Prop
hasSess (Effect e) (Session s) = Prop $ do
  sr <- view sessRel
  m <- lift $ mkApp sr [e]
  lift $ mkEq m s

hasIdx :: Integral a => Effect -> a -> Prop
hasIdx (Effect e) idx = Prop $ do
  ir <- view idxRel
  m <- lift $ mkApp ir [e]
  n <- lift $ mkInt idx
  lift $ mkEq m n

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

  -- Session order only relates effects from the same session. Otherwise, they
  -- are unrelated by session order.
  assertProp $ forall_ $ \ x -> forall_ $ \ y ->
    ite (isInSameSess x y /\ (not_ $ sameEffect x y))
    {- then -} (so x y \/ so y x)
    {- else -} (not_ $ so x y \/ so y x)

  -- Session order is transitive
  assertProp $ forall_ $ \ x -> forall_ $ \ y -> forall_ $ \ z ->
    (so x y /\ so y z) ==> (so x z)

  -- The index of an effect is always >= 1
  let _1 = (IntVal . lift . mkInt) 1
  assertProp $ forall_ $ \ x -> forallInt $ \ i -> (i ==* idxOf x) ==> (i >=* _1)

  -- Existence of previous effects in the same session.
  -- For any effect with a index greater than 1, there exists an effect in the
  -- same session, with an index that is one less, and which precedes the
  -- original effect in session order.
  assertProp $ forall_ $ \ a ->
    (idxOf a >* _1) ==> (exists_ $ \b -> (isInSameSess a b /\ (idxOf b ==* (idxOf a -* _1)) /\ so b a))

  -- If two effects are ordered by so, then their indices are ordered by <
  assertProp $ forall_ $ \a -> forall_ $ \b -> (so a b) ==> (idxOf a <* idxOf b)

  -- Two effects have the same address iff they are the same
  assertProp $ forall_ $ \a -> forall_ $ \b -> ((idxOf a ==* idxOf b) /\ isInSameSess a b) ==> sameEffect a b
  assertProp $ forall_ $ \a -> forall_ $ \b -> sameEffect a b ==> ((idxOf a ==* idxOf b) /\ isInSameSess a b)

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
  hbRel <- mkFreshFuncDecl "hb" [effectSort, effectSort] boolSort

  return $ PropState effectSort sessSort visRel soRel hbRel sessRel idxRel

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

assertRtProp :: Prop -> StateT RtState Z3 ()
assertRtProp prop = do
  ps <- use propState
  lift $ runReaderT (assertProp prop) ps

addEffect :: Addr -> StateT RtState Z3 (Effect, Session)
addEffect addr = do
  let Addr sid idx = addr
  ps <- use propState
  sm <- use sessMap

  -- Create the session if it does not exist
  sess <- case sm ^.at sid of
      Nothing -> do
        -- Session id does not exist. Create it.
        sess <- lift $ Session <$> (mkFreshConst "sid_" $ ps^.sessSort)
        sessMap .= (at sid ?~ sess $ sm)
        return sess
      Just sess -> return sess

  -- Create the effect
  eff <- lift $ Effect <$> (mkFreshConst "eff_" $ ps^.effSort)
  em <- use effMap
  effMap .= (at addr ?~ eff $ em)

  -- Assert that the effect belongs to the session
  assertRtProp $ hasSess eff sess
  -- Assert that the effect this index
  assertRtProp $ hasIdx eff idx

  return (eff, sess)

addPrevToUnknownIfNotKnown :: Addr -> StateT RtState Z3 ()
addPrevToUnknownIfNotKnown addr = do
  {- If the previous effect is not known, then it is unknown.
   - Expect: Known effects are processed in session order.
   - XXX KC: improve this solution. Step wise addition.
   -}
  em <- use effMap
  let Addr sid idx = addr
  if idx > 1 && (not $ M.member (Addr sid $ idx - 1) em)
  then addUnknownEffect (Addr sid (idx - 1))
  else return ()

memberProp :: FuncDecl -> Effect -> Prop
memberProp set (Effect eff) = Prop $ lift $ mkApp set [eff]

-- Extend a set with a effect that is not already a member. Fails if the effect
-- is already a member.
extendSet :: FuncDecl -> String -> Effect -> StateT RtState Z3 FuncDecl
extendSet set str (Effect eff) = do
  ps <- use propState
  bs <- lift $ mkBoolSort
  -- Assert that the effect does not belong to the original set. This asserts
  -- the freshness of the effect.
  assertRtProp $ not_ $ memberProp set $ Effect eff
  newSet <- lift $ mkFreshFuncDecl str [ps^.effSort] bs
  assertRtProp $ forall_ $ \a -> ite (sameEffect a $ Effect eff) true (memberProp set a)
  return newSet

addKnownEffect :: (Addr, S.Set Addr) -> StateT RtState Z3 ()
addKnownEffect (addr, visSet) = do
  (eff, sess) <- addEffect addr
  -- Extend known set with eff
  ks <- use knownSet
  newKs <- extendSet ks "known" eff
  knownSet .= newKs

  -- If the previous effect is not known, then it is unknown. Presupposes that
  -- effects are added in session order.
  addPrevToUnknownIfNotKnown addr

  -- Assert visibility. Only those effects in the visibility are visible to
  -- eff, everything else is not visible. Expect: all unknown effects are in
  -- effMap
  em <- use effMap
  let cond = orList $ map (\addr -> sameEffect eff $ fromJust $ em ^.at addr) $ S.toList visSet
  assertRtProp $ forall_ $ \a -> ite cond (vis a eff) (not_ $ vis a eff)

addUnknownEffect :: Addr -> StateT RtState Z3 ()
addUnknownEffect addr = do
  (eff, sess) <- addEffect addr
  -- Extend unknown set with eff
  us <- use unknownSet
  newUs <- extendSet us "unknown" eff
  unknownSet .= newUs
  -- Assert empty visibility set
  assertRtProp $ forall_ $ \a -> (not_ $ vis a eff)

isContextReady :: Ctxt4Z3 -> Addr -> Spec -> IO (Maybe Ctxt4Z3)
isContextReady ctxt curAddr spec = evalZ3 $ do
  ps <- mkPropState

  -- Assert basic axioms
  runReaderT assertBasicAxioms ps

  -- Build known, unknown relations and RtState
  boolSort <- mkBoolSort
  ks <- mkFreshFuncDecl "known" [ps^.effSort] boolSort
  us <- mkFreshFuncDecl "unknown" [ps^.effSort] boolSort
  let assertEmptiness = do {
    assertProp $ forall_ $ \a -> (not_ $ memberProp ks a);
    assertProp $ forall_ $ \a -> (not_ $ memberProp us a)
  }
  runReaderT assertEmptiness ps

  let rts = RtState ps M.empty M.empty ks us

  -- Build the known & unknown set from the context
  let (ctxtKnownMap, ctxtUnknownSet) = getKUSets ctxt
  let isReadyBool = do
      -- Incorporate current effect
      (curEff, _) <- addEffect curAddr
      addPrevToUnknownIfNotKnown curAddr

      -- Load the known and unknown sets
      mapM_ addUnknownEffect $ S.toList ctxtUnknownSet
      mapM_ addKnownEffect $ M.toList ctxtKnownMap

      -- Helper definitions
      ks <- use knownSet
      let known = memberProp ks
      us <- use unknownSet
      let unknown = memberProp us

      -- source of a happens-before relation belongs to either known or unknown
      assertRtProp $ forall_ $ \a -> forall_ $ \b -> hb a b ==> (known a \/ unknown a)
      -- hb is constructed out of so and vis edges
      assertRtProp $ forall_ $ \a -> forall_ $ \b -> hb a b ==>
        (so a b \/ vis a b \/ (exists_ $ \c -> hb a c /\ hb c b))

      -- Relate known and unknown
      assertRtProp $ forall_ $ \a -> unknown a ==> (not_ $ known a)
      assertRtProp $ forall_ $ \a -> known a ==> (not_ $ unknown a)

      lift $ push
      -- An effect is visible iff it is known
      assertRtProp $ forall_ $ \a -> ite (known a) (vis a curEff) (not_ $ vis a curEff)
      assertRtProp . not_ . spec $ curEff
      res <- lift $ check
      lift $ pop 1
      return $ (res2Bool res, curEff)

  let getReadySubKnown curEff = do
      -- helper definitions
      ks <- use knownSet
      let known = memberProp ks
      us <- use unknownSet
      let unknown = memberProp us
      -- Declare subKnown set
      subKnownSet <- lift $ mkFreshFuncDecl "sub-known" [ps^.effSort] boolSort
      let subKnown = memberProp subKnownSet
      -- Define subKnown set
      assertRtProp $ forall_ $ \a -> subKnown a ==> known a
      assertRtProp $ forall_ $ \a -> known a /\ (not_ $ exists_ $ \b -> unknown b /\ hb b a) ==> subKnown a

      lift $ push
      assertRtProp $ forall_ $ \a -> ite (subKnown a) (vis a curEff) (not_ $ vis a curEff)
      assertRtProp . spec $ curEff
      res <- lift $ check
      lift $ pop 1
      return $ (res, subKnownSet)

  ((res, curEff), rts) <- runStateT isReadyBool rts
  -- If the context is ready, then return the original context
  if res then do
    liftIO $ print "known = SUCCESS"
    return $ Just ctxt
  -- Else, check if there exists a sub-context which is ready
  else do
    ((res, subKnownSet), rts) <- runStateT (getReadySubKnown curEff) rts
    case res of
      Sat -> do
        liftIO $ print "Sub-Known = SUCCESS"
        undefined
      Unsat -> do
        liftIO $ print "Sub-Known = FAILURE"
        undefined

  where
    getKUSets ctxt = foldl kuFoo (M.empty, S.empty) ctxt

    kuFoo (known, unknown) (Row4Z3 s a v) =
      let known = M.insert (Addr s a) v known
          unknown = S.difference (S.union unknown v) (M.keysSet known)
      in (known, unknown)
