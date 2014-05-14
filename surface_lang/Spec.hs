{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, GADTs, DeriveFunctor, RankNTypes,
             TypeSynonymInstances, EmptyDataDecls, DoAndIfThenElse #-}

module Spec
(
  -- Types
  Prop, Effect, Spec,
  -- Types: debug expose
  Row4Z3(..), Ctxt4Z3, Addr(..),

  -- Spec builders
  forall_, exists_, true, false, not_, (/\), (\/), (==>), sameEffect, vis, so,
  hb, sortOf, ite, sameAttr, distinctEffects, isInSameSess,

  -- Queries
  isAvailable,
  isCoordFree,
  isWellTyped,
  isContextReady
) where

import Language.Haskell.TH
import Z3.Monad hiding (mkFreshFuncDecl, mkFreshConst, assertCnstr, Sort, push,
                        pop, check)
import qualified Z3.Monad as Z3M (Sort, mkFreshFuncDecl, mkFreshConst,
                                  assertCnstr, push, pop, check)
import Control.Applicative hiding ((<*))
import Data.List (find)
import Control.Lens hiding (Effect)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S
import Types
import System.IO

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

#define DEBUG_SHOW
#define DEBUG_CHECK
#define DEBUG_SANITY

check :: Z3 Result
#ifdef DEBUG_SHOW
check = do
  liftIO $ do
    putStrLn "(check-sat)"
    hFlush stdout
    hFlush stderr
  Z3M.check
#else
check = Z3M.check
#endif

push :: Z3 ()
#ifdef DEBUG_SHOW
push = do
  liftIO $ do
    putStrLn "(push)"
    hFlush stdout
    hFlush stderr
  Z3M.push
#else
push = Z3M.push
#endif

pop :: Int -> Z3 ()
#ifdef DEBUG_SHOW
pop n | n /= 1 = error "pop"
pop 1 = do
  liftIO $ do
    putStrLn "(pop)"
    hFlush stdout
    hFlush stderr
  Z3M.pop 1
#else
pop = Z3M.pop
#endif

assertCnstr :: String -> AST -> Z3 ()
#ifdef DEBUG_SHOW
assertCnstr name ast = do
  setASTPrintMode Z3_PRINT_SMTLIB2_COMPLIANT
  astStr <- astToString ast
  liftIO $ do
    putStrLn $ ";; --------------------------------"
    putStrLn $ ";; Assert: " ++ name
    putStrLn $ "(assert " ++ astStr ++ ")"
    hFlush stdout
    hFlush stderr
  Z3M.assertCnstr ast
  #ifdef DEBUG_CHECK
  push
  r <- check
  liftIO $ putStrLn $ ";; Assert Result: " ++ (show r)
  pop 1
  #endif
#else
assertCnstr s a = Z3M.assertCnstr a
#endif

mkFreshFuncDecl :: String -> [Z3M.Sort] -> Z3M.Sort -> Z3 FuncDecl
#ifdef DEBUG_SHOW
mkFreshFuncDecl s args res = do
  setASTPrintMode Z3_PRINT_SMTLIB2_COMPLIANT
  fd <- Z3M.mkFreshFuncDecl s args res
  fdStr <- funcDeclToString fd
  liftIO $ putStrLn $ ";; --------------------------------\n" ++ fdStr ++ "\n"
  liftIO $ hFlush stdout
  liftIO $ hFlush stderr
  return fd
#else
mkFreshFuncDecl = Z3M.mkFreshFuncDecl
#endif

mkFreshConst :: String -> Z3M.Sort -> Z3 AST
#ifdef DEBUG_SHOW
mkFreshConst str srt = do
  c <- Z3M.mkFreshConst str srt
  cstr <- astToString c
  srtstr <- sortToString srt
  liftIO $ putStrLn $ ";; --------------------------------"
  liftIO $ putStrLn $ "(declare-const " ++ cstr ++ " " ++ srtstr ++ ")\n"
  liftIO $ hFlush stdout
  liftIO $ hFlush stderr
  return c
#else
mkFreshConst = Z3M.mkFreshConst
#endif

#ifdef DEBUG_SANITY
debugCheck str =
  lift $ push >> check >>= (\r -> when (r == Unsat) $ error str) >> pop 1
#else
debugCheck str = return ()
#endif

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

assertProp :: String -> Prop -> ReaderT PropState Z3 ()
assertProp str = unProp >=> (lift . (assertCnstr str))

assertBasicAxioms :: ReaderT PropState Z3 ()
assertBasicAxioms = do

  -- Visibility is asymmetric
  assertProp "VIS_ASYMM" $ forall_ $ \ x -> forall_ $ \ y -> vis x y ==> (not_ $ vis y x)

  -- Session order is asymmetric
  assertProp "SO_ASYMM" $ forall_ $ \ x -> forall_ $ \ y -> so x y ==> (not_ $ so y x)

  -- Session order only relates effects from the same session. Otherwise, they
  -- are unrelated by session order.
  assertProp "SO_DISJ_TO" $ forall_ $ \ x -> forall_ $ \ y ->
    ite (isInSameSess x y /\ (not_ $ sameEffect x y))
    {- then -} (so x y \/ so y x)
    {- else -} (not_ $ so x y \/ so y x)

  -- Session order is transitive
  assertProp "SO_TRANS" $ forall_ $ \ x -> forall_ $ \ y -> forall_ $ \ z ->
    (so x y /\ so y z) ==> (so x z)

  -- The index of an effect is always >= 1
  let _1 = (IntVal . lift . mkInt) 1
  assertProp "IDX_GE_1" $ forall_ $ \ x -> forallInt $ \ i -> (i ==* idxOf x) ==> (i >=* _1)

  -- Existence of previous effects in the same session.
  -- For any effect with a index greater than 1, there exists an effect in the
  -- same session, with an index that is one less, and which precedes the
  -- original effect in session order.
  assertProp "EXIST_PREV_IDX_GT_1" $ forall_ $ \ a ->
    (idxOf a >* _1) ==> (exists_ $ \b -> (isInSameSess a b /\ (idxOf b ==* (idxOf a -* _1)) /\ so b a))

  -- If two effects are ordered by so, then their indices are ordered by <
  assertProp "SO_FOLLOWS_IDX" $ forall_ $ \a -> forall_ $ \b -> (so a b) ==> (idxOf a <* idxOf b)

  -- Two effects have the same address iff they are the same
  assertProp "EQ_ADDR_EFF_1" $ forall_ $ \a -> forall_ $ \b -> ((idxOf a ==* idxOf b) /\ isInSameSess a b) ==> sameEffect a b
  assertProp "EQ_ADDR_EFF_2" $ forall_ $ \a -> forall_ $ \b -> sameEffect a b ==> ((idxOf a ==* idxOf b) /\ isInSameSess a b)

  -- Happens-before follows visibility and session order
  assertProp "HB_FOLLOWS_VIS_N_SO" $ forall_ $ \a -> forall_ $ \b -> (vis a b \/ so a b) ==> hb a b
  -- Happens-before is transitive
  assertProp "HB_TRANS" $ forall_ $ \ x -> forall_ $ \ y -> forall_ $ \ z ->
    (hb x y /\ hb y z) ==> (hb x z)
  -- Happens-before is acyclic
  assertProp "HB_ASYMM" $ forall_ $ \ x -> forall_ $ \ y -> hb x y ==> (not_ $ hb y x)


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

res2Bool :: Result -> Bool
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
      assertProp "AV_VIS" $ forall_ $ \a -> forall_ $ \b -> vis a eff ==> avr a eff
      assertProp "AV_VIS_SO" $ forall_ $ \a -> forall_ $ \b -> ((vis a b \/ so a b) /\ avr b eff) ==> (avr a eff)
      assertProp "AV_IMPL_VIS" $ forall_ $ \a -> avr a eff ==> vis a eff

      -- Assert validity of the given specification under the extended context
      (assertProp "AV_CHECK") . not_ . s $ eff
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
      assertProp "CF_VIS_SO" $ forall_ $ \a -> (vis a eff \/ so a eff) ==> hb a eff
      assertProp "CF_TRANS" $ forall_ $ \a -> forall_ $ \b -> ((so a b \/ vis a b) /\ hb b eff) ==> hb a eff
      assertProp "CF_IMPL_VIS" $ forall_ $ \a -> hb a eff ==> vis a eff

      -- Assert validity of the given specification under causal visibility
      (assertProp "CF_CHECK") . not_ . s $ eff
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
      assertProp "SC_TO" $ forall_ $ \a -> forall_ $ \b -> to a b \/ to b a \/ sameEffect a b
      assertProp "SC_FOLLOWS_TO" $ forall_ $ \a -> forall_ $ \b -> so a b ==> to a b
      assertProp "SC_VIS" $ forall_ $ \a -> forall_ $ \b -> to a b ==> vis a b

      -- Assert the validity of the given specification under sequential consistency
      (assertProp "WT_CHECK") . not_ . forall_ $ s
      lift $ res2Bool <$> check

assertRtProp :: String -> Prop -> StateT RtState Z3 ()
assertRtProp str prop = do
  ps <- use propState
  lift $ runReaderT (assertProp str prop) ps

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
  assertRtProp "ADD_EFF_SESS" $ hasSess eff sess
  -- Assert that the effect this index
  assertRtProp "ADD_EFF_IDX" $ hasIdx eff idx

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
extendSet set str eff = do
  ps <- use propState
  bs <- lift $ mkBoolSort
  -- Declare the new set
  newSet <- lift $ mkFreshFuncDecl str [ps^.effSort] bs
  let falseBranch quantifiedEff = Prop $ do
        a <- unProp $ memberProp newSet quantifiedEff
        b <- unProp $ memberProp set quantifiedEff
        lift $ mkEq a b
  assertRtProp "EXT_SET" $ forall_ $ \a -> ite (sameEffect a eff) (memberProp newSet eff) $ falseBranch a
  return newSet

addKnownEffect :: (Addr, S.Set Addr) -> StateT RtState Z3 ()
addKnownEffect (Addr _ 0, _) = error "addKnownEffect : index = 0"
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
  if S.size visSet == 0
  then assertRtProp "ADD_K_EFF_EMP_VIS" $ forall_ $ \a -> not_ $ vis a eff
  else do
    let cond = orList $ map (\addr -> sameEffect eff $ fromJust $ em ^.at addr) $ S.toList visSet
    assertRtProp "ADD_K_EFF_NEMP_VIS" $ forall_ $ \a -> ite cond (vis a eff) (not_ $ vis a eff)

addUnknownEffect :: Addr -> StateT RtState Z3 ()
addUnknownEffect (Addr _ 0) = error "addUnknownEffect : index = 0"
addUnknownEffect addr = do
  (eff, sess) <- addEffect addr
  -- Extend unknown set with eff
  us <- use unknownSet
  newUs <- extendSet us "unknown" eff
  unknownSet .= newUs
  -- Assert empty visibility set
  assertRtProp "ADD_U_EFF" $ forall_ $ \a -> (not_ $ vis a eff)

isContextReady :: Ctxt4Z3 -> Addr -> Spec -> IO (Maybe [Addr])
isContextReady ctxt curAddr spec = evalZ3 $ do
  ps <- mkPropState

  -- Assert basic axioms
  runReaderT assertBasicAxioms ps

  -- Build known, unknown relations and RtState
  boolSort <- mkBoolSort
  ks <- mkFreshFuncDecl "known" [ps^.effSort] boolSort
  us <- mkFreshFuncDecl "unknown" [ps^.effSort] boolSort
  let assertEmptiness = do {
    assertProp "K_EMP" $ forall_ $ \a -> (not_ $ memberProp ks a);
    assertProp "UK_EMP" $ forall_ $ \a -> (not_ $ memberProp us a)
  }
  runReaderT assertEmptiness ps

  let rts = RtState ps M.empty M.empty ks us

  -- Build the known & unknown set from the context
  let (ctxtKnownMap, ctxtUnknownSet) = getKUSets ctxt

  let isReadyBool = do
      -- Load the known and unknown sets
      mapM_ addUnknownEffect $ S.toList ctxtUnknownSet
      mapM_ addKnownEffect $ M.toList ctxtKnownMap

      -- Incorporate current effect
      (curEff, _) <- addEffect curAddr
      addPrevToUnknownIfNotKnown curAddr

      -- Helper definitions
      ks <- use knownSet
      let known = memberProp ks
      us <- use unknownSet
      let unknown = memberProp us

      -- source of a happens-before relation belongs to either known or unknown
      assertRtProp "HB_ORIG_K_U" $ forall_ $ \a -> forall_ $ \b -> hb a b ==> (known a \/ unknown a)
      -- hb is constructed out of so and vis edges
      assertRtProp "HB_BRK_VIS_SO" $ forall_ $ \a -> forall_ $ \b -> hb a b ==>
        (so a b \/ vis a b \/ (exists_ $ \c -> hb a c /\ hb c b))

      -- Relate known and unknown
      assertRtProp "UK_IMPL_N_K" $ forall_ $ \a -> unknown a ==> (not_ $ known a)
      assertRtProp "K_IMPL_N_UK" $ forall_ $ \a -> known a ==> (not_ $ unknown a)

      debugCheck "Error : isReadyBool"

      lift $ push
      -- An effect is visible iff it is known
      assertRtProp "K_IMPL_VIS" $ forall_ $ \a -> ite (known a) (vis a curEff) (not_ $ vis a curEff)
      (assertRtProp "K_CHECK") . not_ . spec $ curEff
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
      assertRtProp "SK_SUBSET_K" $ forall_ $ \a -> subKnown a ==> known a
      assertRtProp "SK_INCL_K" $ forall_ $ \a -> known a /\ (not_ $ exists_ $ \b -> unknown b /\ hb b a) ==> subKnown a

      debugCheck "Error : getReadySubKnown"

      lift $ push
      -- An effect is visible iff it is sub-known
      assertRtProp "SK_IMPL_VIS" $ forall_ $ \a -> ite (subKnown a) (vis a curEff) (not_ $ vis a curEff)
      (assertRtProp "SK_CHECK") . spec $ curEff
      res <- lift $ check
      lift $ pop 1
      case res of
        Sat -> return $ Just subKnownSet
        Unsat -> return Nothing


  ((res, curEff), rts) <- runStateT isReadyBool rts
  -- If the context is ready, then return the original context
  if res then do
    return $ Just $ M.keys ctxtKnownMap
  -- Else, check if there exists a sub-context which is ready
  else do
    (res, rts) <- runStateT (getReadySubKnown curEff) rts
    case res of
      Nothing -> do
        return Nothing
      Just subKnownSet -> do
        (Sat, Just model) <- getModel
        subKnownAddrs <- filterM (isInSubKnown (rts^.effMap) subKnownSet) $ M.keys ctxtKnownMap
        return $ Just subKnownAddrs
  where
    isInSubKnown em subKnownSet addr = do
      let (Effect eff) = fromJust $ em ^.at addr
      v <- mkApp subKnownSet [eff] >>= getBool
      case v of
        (Just True) -> return True
        (Just False) -> return False
        Nothing -> error "isInSubKnown"

    getKUSets ctxt = foldl kuFoo (M.empty, S.empty) ctxt

    kuFoo (known, unknown) (Row4Z3 s a v) =
      let known' = M.insert (Addr s a) v known
          unknown' = S.difference (S.union unknown v) (M.keysSet known')
      in (known', unknown')
