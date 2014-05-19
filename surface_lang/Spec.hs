{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, GADTs, DeriveFunctor, RankNTypes,
             TypeSynonymInstances, EmptyDataDecls, DoAndIfThenElse #-}

module Spec
(
  -- Types
  Prop, Effect, Spec,
  -- Types: debug expose
  Z3Row(..), Z3Ctxt, Addr(..),

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
                        pop, check, getModel)
import qualified Z3.Monad as Z3M (Sort, mkFreshFuncDecl, mkFreshConst,
                                  assertCnstr, push, pop, check, getModel)
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

  ------------
  -- Relations
  ------------
  -- Visibility relation
  _visRel  :: FuncDecl, -- Effect -> Effect -> Bool
  -- Session order relation
  _soRel   :: FuncDecl, -- Effect -> Effect -> Bool
  -- Happens-before relation
  _hbRel   :: FuncDecl, -- Effect -> Effect -> Bool
  -- Session relation
  _sessRel :: FuncDecl, -- Effect -> Int
  -- Assetions
  _assertions :: [AST]
}

makeLenses ''PropState

newtype Effect = Effect { unEffect :: AST }
newtype Session = Session { unSession :: AST }

data EffData = Current | Known (S.Set Addr) | Unknown deriving (Show, Eq, Ord)

newtype Prop = Prop { unProp :: StateT PropState Z3 AST }

data Sort
class Attr a
type Spec = Effect -> Prop

-------------------------------------------------------------------------------
-- Helper

#define DEBUG_SHOW
-- #define DEBUG_CHECK
-- #define DEBUG_SANITY

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

getModel :: Z3 (Result, Maybe Model)
#ifdef DEBUG_SHOW
getModel = do
  liftIO $ do
    putStrLn "(check-sat) ;; get-model"
    hFlush stdout
    hFlush stderr
  Z3M.getModel
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

mkEffectConst :: StateT PropState Z3 Effect
mkEffectConst = do
  as <- use effSort
  lift $ Effect <$> mkFreshConst "Eff_" as

forall_ :: (Effect -> Prop) -> Prop
forall_ f = Prop $ do
  as <- use effSort
  qvConst <- lift $ mkFreshConst "FA_E_" as
  qv <- lift $ toApp qvConst
  body <- unProp $ f (Effect qvConst)
  lift $ mkForallConst [] [qv] body

exists_ :: (Effect -> Prop) -> Prop
exists_ f = Prop $ do
  as <- use effSort
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
  vr <- use visRel
  lift $ mkApp vr [a1,a2]

so :: Effect -> Effect -> Prop
so (Effect a1) (Effect a2) = Prop $ do
  sr <- use soRel
  lift $ mkApp sr [a1, a2]

hb :: Effect -> Effect -> Prop
hb (Effect a1) (Effect a2) = Prop $ do
  hr <- use hbRel
  lift $ mkApp hr [a1, a2]

sortOf :: Effect -> Sort -> Prop
sortOf = undefined

sameAttr :: Effect -> Effect -> Prop
sameAttr = undefined

isInSameSess :: Effect -> Effect -> Prop
isInSameSess (Effect a) (Effect b) = Prop $ do
  sr <- use sessRel
  s1 <- lift $ mkApp sr [a]
  s2 <- lift $ mkApp sr [b]
  lift $ mkEq s1 s2

ite :: Prop -> Prop -> Prop -> Prop
ite (Prop p1) (Prop p2) (Prop p3) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  ast3 <- p3
  lift $ mkIte ast1 ast2 ast3

hasSess :: Integral a => Effect -> a -> Prop
hasSess (Effect a) i = Prop $ do
  sr <- use sessRel
  iv <- lift $ mkInt i
  s <- lift $ mkApp sr [a]
  lift $ mkEq s iv

-------------------------------------------------------------------------------
-- Queries

assertProp :: String -> Prop -> StateT PropState Z3 AST
assertProp str (Prop prop) = do
  ast <- prop
  lift $ assertCnstr str ast
  asl <- use assertions
  assertions .= ast:asl
  return ast

assertProp2 :: String -> Prop -> StateT PropState Z3 AST
assertProp2 str (Prop prop) = do
  ast <- prop
  lift $ assertCnstr str ast
  asl <- use assertions
  return ast


assertBasicAxioms :: StateT PropState Z3 ()
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

  -- Session relation
  assertProp "SO_SESS" $ forall_ $ \a -> forall_ $ \b -> so a b \/ so b a ==> isInSameSess a b

  -- Happens-before follows visibility and session order
  assertProp "HB_FOLLOWS_VIS_N_SO" $ forall_ $ \a -> forall_ $ \b -> (vis a b \/ so a b) ==> hb a b
  -- Happens-before is transitive
  assertProp "HB_TRANS" $ forall_ $ \ x -> forall_ $ \ y -> forall_ $ \ z ->
    (hb x y /\ hb y z) ==> (hb x z)
  -- Happens-before is acyclic
  assertProp "HB_ASYMM" $ forall_ $ \ x -> forall_ $ \ y -> hb x y ==> (not_ $ hb y x)
  return ()


mkPropState :: Z3 PropState
mkPropState = do
  effectSort <- mkUninterpretedSort =<< mkStringSymbol "Effect"
  boolSort <- mkBoolSort
  intSort <- mkIntSort

  visRel <- mkFreshFuncDecl "vis" [effectSort, effectSort] boolSort
  soRel <- mkFreshFuncDecl "so" [effectSort, effectSort] boolSort
  sessRel <- mkFreshFuncDecl "sess" [effectSort] intSort
  hbRel <- mkFreshFuncDecl "hb" [effectSort, effectSort] boolSort

  return $ PropState effectSort visRel soRel hbRel sessRel []

res2Bool :: Result -> Bool
res2Bool Unsat = True
res2Bool Sat = False

-- http://rise4fun.com/Z3/v6jF
isAvailable :: Spec -> IO Bool
isAvailable s = evalZ3 $ do
  ps <- mkPropState
  (res, _) <- runStateT core ps
  return res
  where
    core = do
      -- assert basic axioms
      assertBasicAxioms

      -- Declare availability relation
      es <- use effSort
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
  (res, ps) <- runStateT core ps
  return res
  where
    core = do
      -- assert basic axioms
      assertBasicAxioms

      -- Declare a happens-before relation
      es <- use effSort
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
  (res, _) <- runStateT core ps
  return res
  where
    core = do
      -- assert basic axioms
      assertBasicAxioms

      -- Declare a total order relation
      es <- use effSort
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

isContextReady :: Z3Ctxt -> Addr -> Spec -> IO (Maybe [Addr])
isContextReady ctxt curAddr spec = evalZ3WithInterpolationContext $ do
  let effMap = buildKU ctxt
  let Addr s i = curAddr
  let (_,_,infoMap) = M.foldlWithKey (\(prevSess, prevIdx, m) addr v ->
             insertMissing addr v prevSess prevIdx m) (Nothing, Nothing, M.empty)
             (M.insert curAddr Current effMap)

  ps <- mkPropState
  (_,ps) <- runStateT assertBasicAxioms ps

  let effMapHelper addr = do
      eff <- Effect <$> (mkFreshConst "eff" $ ps^.effSort)
      return (addr, eff)
  effMapList <- mapM (\addr -> effMapHelper addr) $ M.keys infoMap
  let effMap = M.fromList effMapList

  let card = assertProp "CARDINALITY" $ forall_ $ \a -> orList (map (sameEffect a) $ M.elems effMap)
  (_, ps) <- runStateT card ps

  let distinct = assertProp "DISTINCT" $ distinctEffects $ M.elems effMap
  (_, ps) <- runStateT distinct ps

  let so = foldM_ (\acc (k,v) -> soHelper k v acc) Nothing (M.toList effMap)
  (_, ps) <- runStateT so ps

  let vis = mapM_ (\(k,v) -> visHelper (infoMap,effMap) k v) (M.toList effMap)
  (_,ps) <- runStateT vis ps

  boolSort <- mkBoolSort
  knownRel <- mkFreshFuncDecl "known" [ps^.effSort] boolSort
  let known = mkMember knownRel
  unknownRel <- mkFreshFuncDecl "unknown" [ps^.effSort] boolSort
  let unknown = mkMember unknownRel

  let (knownEffs, unknownEffs) = M.foldWithKey (partitionHelper effMap) (M.empty, M.empty) infoMap
  let assertKnown = assertProp "KNOWN" $ forall_ $ \a ->
        if M.size knownEffs == 0 then (not_ $ known a)
        else ite (orList $ map (sameEffect a) $ M.elems knownEffs) (known a) (not_ $ known a)
  (_,ps) <- runStateT assertKnown ps
  let assertUnknown = assertProp "UNKNOWN" $ forall_ $ \a ->
        if M.size unknownEffs == 0 then (not_ $ unknown a)
        else ite (orList $ map (sameEffect a) $ M.elems unknownEffs) (unknown a) (not_ $ unknown a)
  (_,ps) <- runStateT assertUnknown ps

  let curEff = fromJust $ effMap ^.at curAddr
  (_,ps) <- runStateT (auxAxioms known unknown curEff) ps

  (res,ps) <- runStateT (mainKnownAxioms known unknown curEff spec) ps
  case res of
    Unsat -> return $ Just $ M.keys knownEffs
    Sat -> do
      subKnownRel <- mkFreshFuncDecl "subKnown" [ps^.effSort] boolSort
      let subKnown = mkMember subKnownRel
      (res,ps) <- runStateT (mainSubknownAxioms subKnown known unknown curEff spec) ps
      case res of
        Nothing -> do
          return Nothing
        Just model -> do
          sk <- foldM (belongsToSubknown subKnownRel model) [] $ M.toList knownEffs
          return $ Just sk

  where
    mkMember fd (Effect eff) = Prop $ lift $ mkApp fd [eff]

    belongsToSubknown skRel model acc (addr, Effect eff) = do
      app <- mkApp skRel [eff]
      res <- eval model app
      case res of
        Nothing -> return acc
        Just ast -> do
          res <- getBool ast
          case res of
            Just True -> return $ addr:acc
            otherwise -> return acc

    mainSubknownAxioms subKnown known unknown cur spec = do
      -- Define subKnown set
      assertProp "SK_SUBSET_K" $ forall_ $ \a -> subKnown a ==> known a
      assertProp "SK_INCL_K" $ forall_ $ \a -> known a /\ (not_ $ exists_ $ \b -> unknown b /\ hb b a) ==> subKnown a
      lift $ push
      -- An effect is visible iff it is sub-known
      f1 <- assertProp "SK_IMPL_VIS" $ forall_ $ \a -> ite (subKnown a) (vis a cur) (not_ $ vis a cur)
      f2 <- (assertProp "SK_CHECK") . spec $ cur
      (res, model) <- lift $ getModel
      case (res,model) of
        (Sat, Just m) -> do
          lift $ pop 1
          return $ Just m
        otherwise -> do
          asl <- use assertions
          lift $ do
            res <- interpolate2 (reverse asl) [f1,f2]
            sres <- mapM astToString res
            liftIO $ mapM_ putStrLn sres
            pop 1
            return Nothing

    mainKnownAxioms known unknown cur spec = do
      lift $ push
      -- An effect is visible iff it is known
      assertProp2 "K_IMPL_VIS" $ forall_ $ \a -> ite (known a) (vis a cur) (not_ $ vis a cur)
      (assertProp2 "K_CHECK") . not_ . spec $ cur
      res <- lift $ check
      lift $ pop 1
      return res

    auxAxioms known unknown cur = do
      -- source of a happens-before relation belongs to either known or unknown
      assertProp "HB_ORIG_K_U" $ forall_ $ \a -> forall_ $ \b -> hb a b ==>
        ((known a \/ unknown a) /\ (known b \/ unknown b \/ sameEffect b cur))
      -- hb is constructed out of so and vis edges
      assertProp "HB_BRK_VIS_SO" $ forall_ $ \a -> forall_ $ \b -> hb a b ==>
        (so a b \/ vis a b \/ (exists_ $ \c -> hb a c /\ hb c b))
      -- Relate known and unknown
      assertProp "UK_IMPL_N_K" $ forall_ $ \a -> unknown a ==> (not_ $ known a)
      assertProp "K_IMPL_N_UK" $ forall_ $ \a -> known a ==> (not_ $ unknown a)

    partitionHelper em addr Current (km,um) = (km,um)
    partitionHelper em addr Unknown (km,um) = (km, M.insert addr (fromJust $ em ^.at addr) um)
    partitionHelper em addr (Known _) (km,um) = (M.insert addr (fromJust $ em ^.at addr) km, um)

    {- buildKU
     ---------
     - Returns a m = M.Map Addr (Maybe [Addr]).
     - if m[x] = Nothing, then x is unknown. Otherwise, x is known, and m[x] is
     - its visibility set.
     -}
    buildKU ctxt =
      let (k, u) = foldl buildKUHelper (M.empty, S.empty) ctxt
          justK = M.map (\a -> Known a) k
      in S.foldl (\m addr -> M.insert addr Unknown m) justK u

    buildKUHelper (known, unknown) (Z3Row s a v) =
      let known2 = M.insert (Addr s a) v known
          unknown2 = S.difference (S.union unknown v) (M.keysSet known2)
      in (known2, unknown2)

    insertMissing (Addr cSess cIdx) v Nothing Nothing m =
      if cIdx /= 1
      then (Just cSess, Just cIdx, M.insert (Addr cSess (cIdx - 1)) Unknown $ M.insert (Addr cSess cIdx) v m)
      else (Just cSess, Just cIdx, M.insert (Addr cSess cIdx) v m)
    insertMissing (Addr cSess cIdx) v (Just pSess) (Just pIdx) m =
      if ((cSess == pSess && cIdx == pIdx + 1) || (cSess /= pSess && cIdx == 1))
      then (Just cSess, Just cIdx, M.insert (Addr cSess cIdx) v m)
      else if ((cSess == pSess && cIdx /= pIdx + 1) || (cSess /= pSess && cIdx /= 1))
      then (Just cSess, Just cIdx, M.insert (Addr cSess (cIdx - 1)) Unknown $ M.insert (Addr cSess cIdx) v m)
      else error "insertMissing"

    -- Convert a (Maybe (S.Set Addr)) to EffData
    mb2ed Nothing = Unknown
    mb2ed (Just v) = Known v

    soHelper (Addr sess _) eff Nothing = do
      assertProp "SO_FIRST" $ forall_ $ \a -> not_ $ so a eff
      assertProp "SESS_FIRST" $ hasSess eff 0
      return $ Just (sess, eff, 0)
    soHelper (Addr cSess _) cEff (Just (pSess, pEff, pSid)) = do
      if pSess == cSess
      then do
        assertProp "SO_MID" $ so pEff cEff
        assertProp "SESS_MID" $ hasSess cEff pSid
        return $ Just (cSess, cEff, pSid)
      else do
        assertProp "SO_LAST" $ forall_ $ \a -> not_ $ so pEff a
        assertProp "SO_FIRST" $ forall_ $ \a -> not_ $ so a cEff
        assertProp "SESS_FIRST" $ hasSess cEff (pSid + 1)
        return $ Just (cSess, cEff, pSid + 1)

    visHelper (infoMap, effMap) addr eff = do
      case fromJust $ infoMap ^.at addr of
        Current -> return ()
        Unknown -> do
          assertProp "UK_VIS" $ forall_ $ \a -> not_ $ vis a eff
          return ()
        Known s -> do
          let visEffs = map (\addr -> fromJust $ effMap ^.at addr) $ S.toList s
          assertProp "K_VIS" $ forall_ $ \a ->
            if length visEffs == 0 then (not_ $ vis a eff)
            else ite (orList $ map (sameEffect a) visEffs) (vis a eff) (not_ $ vis a eff)
          return ()

