{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, GADTs, DeriveFunctor #-}

module Tracer
( -- Types
  SolverEvent,
  SolverAttr,
  Action,
  Session,
  ECD,
  Result (..),
  EventType,
  AttrType,
  FOL,
  Prop,

  -- Axiom builders
  forall_,
  exists_,
  prop,

  true_,
  false_,
  not_,
  or_,
  and_,
  implies_,
  ite_,

  sameAct,
  visTo,
  sessOrd,
  isEvent,
  isInSameSess,
  inActSoup,
  isStrongAct,
  sameAttr,

  -- Consistency annotation
  basicEventual,
  readMyWrites,
  strong,

  -- Execution builder and checker
  newAction,
  newSession,
  checkConsistency,
  liftEvent,
  liftAttr,
  runECD,

  -- Auxiliary functions
  doIO,

) where


import Language.Haskell.TH
import Z3.Monad hiding (Result)
import qualified Z3.Monad as Z3M
import Control.Applicative
import Data.List (find)
import Control.Lens hiding (Action)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Map as Map hiding (map)
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Global strings

actSortStr :: String
actSortStr = "Action"

sessSortStr :: String
sessSortStr = "Session"

evtRecgRelStr :: String
evtRecgRelStr = "evtRecgRel"

visRelStr :: String
visRelStr = "visRel"

soRelStr :: String
soRelStr = "soRel"

sessRelStr :: String
sessRelStr = "sessRel"

attrRelStr :: String
attrRelStr = "AttrRel"

-------------------------------------------------------------------------------
-- Helper functions

-- Monadic composition
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g


-- #define DBG_ASSERT

assertCnstr :: AST -> Z3 ()
#ifdef DBG_ASSERT
assertCnstr ast = do
  setASTPrintMode Z3_PRINT_SMTLIB2_COMPLIANT
  astStr <- astToString ast
  liftIO $ putStrLn $ "--------\nAssert: \n--------\n" ++ astStr ++ "\n"
  Z3M.assertCnstr ast
  r <- check
  liftIO $ putStrLn $ "Assert Result: " ++ (show r)
#else
assertCnstr = Z3M.assertCnstr
#endif

-- Traverse helper
traverseAt :: (Applicative f, Ord k) => k -> (v -> f v) -> Map k v -> f (Map k v)
traverseAt k = at k . traverse

-------------------------------------------------------------------------------
-- Types

class Show a => SolverEvent a
class Show a => SolverAttr a

type ECD a = StateT Exec Z3 a
type Result = Z3M.Result
type EventType = Z3 Sort
type AttrType = Z3 (Sort, [Sort])
type ConsAnn = (Action -> ECD AST)

newtype EventSort   = EventSort { unEventSort :: Sort }
newtype Action      = Action    { unAct :: AST }
newtype AttrVal     = AttrVal   { unAttrVal :: AST }
newtype Event       = Event     { unEvent :: AST }
newtype Session     = Session   { unSession :: AST }

data AttrInfo = AttrInfo {
  _attrSort :: Sort,            -- Type of this attribute
  _attrSoup :: AST,             -- Set of known constants for this type of
                                -- attribute
  _attrRel  :: FuncDecl,        -- Action -> AttrIdx -> Attr. For example, take
                                -- "key(a) = k". Here, a = Action, key =
                                -- AttrIndex, k = Attr.
  _attrMap  :: Map String AST   -- Map from the name of the constant to the Z3
                                -- constant. The name of the constant is always
                                -- prefixed with the attribute name in otder to
                                -- avoid conflicts.
}


data Exec = Exec { -- Soups
                   _actSoup   :: AST, -- Set Action
                   _sessSoup  :: AST, -- Set Session
                   _strgSoup  :: AST, -- Set Action -> Set of actions with
                                      -- "Strong" consistency annotation.
                   -- Relations
                   _sessRel     :: FuncDecl, -- Action -> Session
                   _evtRecgRel  :: FuncDecl, -- Action -> Event -> Bool
                   _visRel      :: FuncDecl, -- Action -> Action -> Bool
                   _soRel       :: FuncDecl, -- Action -> Action -> Bool
                   -- Sorts
                   _actSort     :: Sort,
                   _sessSort    :: Sort,
                   _evtSort     :: Sort,
                   _attrIdxSort :: Sort,
                   -- Attributes
                   _attrInfoMap :: Map String AttrInfo}

-- First-order logic.
newtype FOL = FOL { unFOL :: ReaderT Exec Z3 AST }

-- Propositional logic type.
newtype Prop = Prop { unProp :: ReaderT Exec Z3 AST }

makeLenses ''Exec
makeLenses ''AttrInfo

-------------------------------------------------------------------------------
-- Logic library

forall_ :: (Action -> FOL) -> FOL
forall_ f = FOL $ do
  as <- view actSort
  qvConst <- lift $ mkFreshConst "FA_" as
  qv <- lift $ toApp qvConst
  body <- unFOL $ f (Action qvConst)
  lift $ mkForallConst [] [qv] body

prop :: Prop -> FOL
prop p = FOL $ unProp p

exists_ :: (Action -> FOL) -> FOL
exists_ f = FOL $ do
  as <- view actSort
  qvConst <- lift $ mkFreshConst "EX_" as
  qv <- lift $ toApp qvConst
  body <- unFOL $ f (Action qvConst)
  lift $ mkExistsConst [] [qv] body

-- Should not be exposed from the module
forallE_ :: (Event -> FOL) -> FOL
forallE_ f = FOL $ do
  es <- view evtSort
  qvConst <- lift $ mkFreshConst "FA_" es
  qv <- lift $ toApp qvConst
  body <- unFOL $ f (Event qvConst)
  lift $ mkForallConst [] [qv] body

-- Should not be exposed from the module
forallAI_ :: (AttrVal -> FOL) -> FOL
forallAI_ f = FOL $ do
  as <- view attrIdxSort
  qvConst <- lift $ mkFreshConst "FA_" as
  qv <- lift $ toApp qvConst
  body <- unFOL $ f (AttrVal qvConst)
  lift $ mkForallConst [] [qv] body


-- Propositional logic constructors
true_ :: Prop
true_ = Prop $ lift mkTrue

false_ :: Prop
false_ = Prop $ lift mkFalse

not_ :: Prop -> Prop
not_ (Prop p) = Prop $ do
  ast <- p
  lift $ mkNot ast

or_ :: Prop -> Prop -> Prop
or_ (Prop p1) (Prop p2) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkOr [ast1, ast2]

and_ :: Prop -> Prop -> Prop
and_ (Prop p1) (Prop p2) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkAnd [ast1, ast2]

implies_ :: Prop -> Prop -> Prop
implies_ (Prop p1) (Prop p2) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  lift $ mkImplies ast1 ast2

sameAct :: Action -> Action -> Prop
sameAct (Action a1) (Action a2) =
  Prop $ lift $ mkEq a1 a2

visTo :: Action -> Action -> Prop
visTo (Action a1) (Action a2) = Prop $ do
  vr <- view visRel
  lift $ mkApp vr [a1,a2]

sessOrd :: Action -> Action -> Prop
sessOrd (Action a1) (Action a2) = Prop $ do
  so <- view soRel
  lift $ mkApp so [a1, a2]

isEvent :: SolverEvent a => Action -> a -> Prop
isEvent (Action a1) e = Prop $ do
  er <- view evtRecgRel
  eventAst <- getEvent e
  lift $ mkApp er [a1, eventAst]

isInSameSess :: Action -> Action -> Prop
isInSameSess (Action a1) (Action a2) = Prop $ do
  sr <- view sessRel
  as1 <- lift $ mkApp sr [a1]
  as2 <- lift $ mkApp sr [a2]
  lift $ mkEq as1 as2

ite_ :: Prop -> Prop -> Prop -> Prop
ite_ (Prop p1) (Prop p2) (Prop p3) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  ast3 <- p3
  lift $ mkIte ast1 ast2 ast3

inActSoup :: Action -> Prop
inActSoup (Action a1) = Prop $ do
  as <- view actSoup
  lift $ mkSetMember a1 as

isStrongAct :: Action -> Prop
isStrongAct (Action a1) = Prop $ do
  as <- view strgSoup
  lift $ mkSetMember a1 as

sameAttr :: SolverAttr a => a -> Action -> Action -> Prop
sameAttr attr (Action a1) (Action a2) = Prop $ do
  attrIdx <- getAttrIdx attr
  aim <- view attrInfoMap
  let ar = (fromJust $ aim ^.at (show attr)) ^. attrRel
  lift $ join $ mkEq <$> (mkApp ar [a1, attrIdx]) <*> (mkApp ar [a2, attrIdx])

-------------------------------------------------------------------------------
-- Consistency annotations

assertFOL :: FOL -> ECD AST
assertFOL (FOL fol) = do
  exec <- get
  lift $ runReaderT fol exec >>= assertCnstr >> mkTrue

basicEventual :: ConsAnn
basicEventual _ = lift $ mkTrue

readMyWrites :: ConsAnn
readMyWrites x = do
  let fol = forall_ $ \ y -> prop $ (y `sessOrd` x) `implies_` (y `visTo` x)
  assertFOL fol

strong :: ConsAnn
strong (Action x) = do
  oldStrgSoup <- use strgSoup
  newStrgSoup <- lift $ mkSetAdd oldStrgSoup x
  strgSoup .= newStrgSoup
  lift $ mkTrue

-------------------------------------------------------------------------------
-- Execution builder

-- Create a Z3 Event Sort that mirrors the Haskell Event Type.
liftEvent :: Name -> ExpQ
liftEvent t = do
  TyConI (DataD _ (typeName::Name) _ constructors _) <- reify t
  let typeNameStr = nameBase typeName
  let consNameStrList = map (\ (NormalC name _) -> nameBase name) constructors
  [| do
       let makeCons consStr = do
           consSym <- mkStringSymbol consStr
           isConsSym <- mkStringSymbol $ "is_" ++ consStr
           mkConstructor consSym isConsSym []
       let makeDatatype = do
           consList <- sequence $ map makeCons consNameStrList
           dtSym <- mkStringSymbol typeNameStr
           mkDatatype dtSym consList
       makeDatatype |]

-- Create Z3 sorts for attributes
liftAttr :: Name -> ExpQ
liftAttr t = do
  TyConI (DataD _ (typeName::Name) _ constructors _) <- reify t
  let typeNameStr = nameBase typeName
  let consNameStrList = map (\ (NormalC name _) -> nameBase name) constructors
  [| do
       let makeCons consStr = do
           consSym <- mkStringSymbol consStr
           isConsSym <- mkStringSymbol $ "is_" ++ consStr
           mkConstructor consSym isConsSym []
       let makeDatatype = do
           consList <- sequence $ map makeCons consNameStrList
           dtSym <- mkStringSymbol typeNameStr
           mkDatatype dtSym consList
       v1 <- makeDatatype
       sortSymList <- sequence $ map mkStringSymbol consNameStrList
       v2 <- sequence $ map mkUninterpretedSort sortSymList
       return (v1, v2) |]


-- Make an empty execution.
mkExec :: Z3 Sort -> Z3 (Sort, [Sort]) -> Z3 Exec
mkExec mkEventSort mkAttrSorts = do
  --------
  -- Sorts
  --------
  actionSym <- mkStringSymbol actSortStr
  actionSort <- mkUninterpretedSort actionSym

  sessionSym <- mkStringSymbol sessSortStr
  sessionSort <- mkUninterpretedSort sessionSym

  eventSort <- mkEventSort
  (attrIdxSort, attrSortList) <- mkAttrSorts
  boolSort <- mkBoolSort

  --------
  -- Soups
  --------
  actionSoup <- mkEmptySet actionSort
  sessionSoup <- mkEmptySet sessionSort
  strongSoup <- mkEmptySet actionSort

  ------------
  -- Relations
  ------------
  evtRecgRel <- mkFreshFuncDecl evtRecgRelStr [actionSort, eventSort] boolSort
  sessRel <- mkFreshFuncDecl sessRelStr [actionSort] sessionSort
  soRel <- mkFreshFuncDecl soRelStr [actionSort, actionSort] boolSort
  visRel <- mkFreshFuncDecl visRelStr [actionSort, actionSort] boolSort

  -------------
  -- Attributes
  -------------
  attrNameStrList <- sequence $ map sortToString attrSortList
  attrInfoList <- sequence $ map
    (\s -> do
             set <- mkEmptySet s
             name <- sortToString s
             rel <- mkFreshFuncDecl (name ++ attrRelStr) [actionSort, attrIdxSort] s
             return $ AttrInfo s set rel $ fromList [])
    attrSortList
  let attrInfoMap = fromList $ zip attrNameStrList attrInfoList

  ------------
  -- Execution
  ------------
  let exec :: Exec = Exec actionSoup sessionSoup strongSoup       -- Soups
                     sessRel evtRecgRel visRel soRel              -- Relations
                     actionSort sessionSort eventSort attrIdxSort -- Sorts
                     attrInfoMap                                  -- attrInfoMap

  -------------
  -- Assertions
  -------------

  -- evtRecgRel: Assertion to create an empty event recognition relation
  -- ∀x.∀y. evtRecgRel x y = false
  assertFOL exec $ forall_ $ \ (Action x) -> forallE_ $ \ (Event y) ->
                    prop $ Prop $ lift $ mkApp evtRecgRel [x,y] >>= mkNot

  -- sessRel: Assertion to associate all actions to a dummy session
  -- ∀x. sessRel x = dummySess
  dummySess <- mkFreshConst "dummySess" sessionSort
  assertFOL exec $ forall_ $ \ (Action x) -> prop $ Prop $ lift $
                     mkEq dummySess =<< mkApp sessRel [x]

  -- soRel: Assertion to create an empty soRel
  assertFOL exec $ forall_ $ \(Action x) -> forall_ $ \(Action y) ->
                     prop $ Prop $ lift $ mkApp soRel [x,y] >>= mkNot

  -- attrRel: Assertions to create empty attrRels
  mapM_ (\ (AttrInfo sort _ rel _) -> do
          attrName <- sortToString sort
          sym <- mkStringSymbol $ "defVal" ++ attrName
          dummyAttr <- mkConst sym sort
          assertFOL exec $ forall_ $ \ (Action x) -> forallAI_ $ \ (AttrVal y) ->
            prop $ Prop $ lift $ mkEq dummyAttr =<< mkApp rel [x,y]) (elems attrInfoMap)

  return exec
  where
    assertFOL exec (FOL fol) = do
      ast <- runReaderT fol exec
      assertCnstr ast


-- Make Attribute
mkAttrVal :: SolverAttr a
          => a      -- Attr
          -> String -- AttrValue. (<Attr name> + show a) will be the constant name.
          -> ECD AttrVal
mkAttrVal attr attrVal = do
  -- Search for the given attribute value in the attrInfoMap
  maybeAttr <- lookup (show attr) attrVal
  case maybeAttr of
    Left ast -> return $ AttrVal ast
    Right attrSort -> do
      let valIndex = show attr ++ attrVal
      sym <- lift $ mkStringSymbol valIndex
      const <- lift $ mkConst sym attrSort
      -- Fetch attrInfo
      aim <- use attrInfoMap
      let ai :: AttrInfo = fromJust $ aim ^.at (show attr)
      -- Extend attribute soup
      let as :: AST = ai ^. attrSoup
      lift $ mkSetMember const as >>= mkNot >>= assertCnstr
      newAs <- lift $ mkSetAdd as const
      attrInfoMap.(traverseAt $ show attr).attrSoup .= newAs
      -- update
      let am :: Map String AST = ai ^. attrMap
      let amNew :: Map String AST = at valIndex .~ Just const $ am
      attrInfoMap.(traverseAt $ show attr).attrMap .= amNew
      -- return result
      return $ AttrVal const
  where
  lookup attr attrVal = do
    exec <- get
    lift $ runReaderT (lookupAttrVal attr attrVal) exec


lookupAttrVal :: String -> String -> ReaderT Exec Z3 (Either AST Sort)
lookupAttrVal attr attrVal = do
  m <- view attrInfoMap
  let ai :: AttrInfo = fromJust $ m ^.at attr
  let am :: Map String AST = ai ^. attrMap
  let ast :: Maybe AST = am ^.at (attr ++ attrVal)
  case ast of
    Just ast -> return $ Left ast
    Nothing -> return $ Right $ ai ^. attrSort


-- Make a new action.
--
-- Invariants
--  (1) Actions are always added in session order.
--  (2) The session to which the action belongs to is already in the session
--      soup.
newAction :: (SolverEvent a, SolverAttr b) =>
          String            -- Action name prefix
          -> a              -- Event
          -> [(b,String)]   -- Attributes
          -> ConsAnn        -- Consistency annotation
          -> Session        -- Session identifier
          -> ECD Action     -- Returns the new action
newAction actStr evt attrList annFun sess = do
  actSort <- use actSort
  sessSort <- use sessSort
  evtSort <- use evtSort

  -- make a new action
  act <- lift $ mkFreshConst actStr actSort

  ---------------------
  -- Extend action soup
  ---------------------
  as <- use actSoup
  lift $ mkSetMember act as >>= mkNot >>= assertCnstr
  newAs <- lift $ mkSetAdd as act
  actSoup .= newAs

  ---------------------
  -- Extend sessRel
  ---------------------
  sr <- use sessRel
  newSr <- lift $ mkFreshFuncDecl sessRelStr [actSort] sessSort
  -- ∀x. if (x = act) then (newSr act = sess) else (newSr x = sr x)
  let trueBranch x = Prop $ lift $
        mkApp newSr [unAct x] >>= mkEq (unSession sess)
  let falseBranch x = Prop $ lift $ join $
        mkEq <$> (mkApp newSr [unAct x]) <*> (mkApp sr [unAct x])
  assertFOL $ forall_ $ \x -> prop $
        ite_ (sameAct x (Action act)) (trueBranch x) (falseBranch x)
  sessRel .= newSr

  ---------------------
  -- Extend soRel
  ---------------------
  sr <- use sessRel
  sor <- use soRel
  newSor <- lift $ mkFreshFuncDecl soRelStr [actSort, actSort] =<< mkBoolSort
  -- ∀x.∀y. if (x ≠ y ∧ y = act ∧ sr(x) = sess) then newSor x y else sor x y
  let conditional x y = Prop $ lift $ do
                          c1 <- mkEq x y >>= mkNot
                          c2 <- mkEq y act
                          c3 <- mkEq (unSession sess) =<< mkApp sr [x]
                          mkAnd [c1,c2,c3]
  let trueBranch x y = Prop $ lift $ mkApp newSor [x,y]
  let falseBranch x y = Prop $ lift $ join $
        mkEq <$> (mkApp newSor [x,y]) <*> (mkApp sor [x,y])
  assertFOL $ forall_ $ \(Action x) -> forall_ $ \(Action y) -> prop $
                ite_ (conditional x y) (trueBranch x y) (falseBranch x y)
  soRel .= newSor

  ---------------------
  -- Extend evtRecgRel
  ---------------------
  err <- use evtRecgRel
  newErr <- lift $ mkFreshFuncDecl evtRecgRelStr [actSort, evtSort] =<< mkBoolSort
  z3Evt <- getZ3Evt evt
  -- ∀x:Action.∀y:Event. if (x = act ∧ y = evt)
  --                     then (newErr x y)
  --                     else (newErr x y = err x y)
  let conditional x y =
        (sameAct x (Action act)) `and_` (Prop $ lift $ mkEq z3Evt $ unEvent y)
  let trueBranch x y = Prop $ lift $ mkApp newErr [unAct x, unEvent y]
  let falseBranch x y = Prop $ lift $ join $
        mkEq <$> (mkApp newErr [unAct x, unEvent y])
             <*> (mkApp err [unAct x, unEvent y])
  assertFOL $ forall_ $ \x -> forallE_ $ \y -> prop $
    ite_ (conditional x y) (trueBranch x y) (falseBranch x y)
  evtRecgRel .= newErr

  ------------------------------------
  -- Assert the consistency annotation
  ------------------------------------
  (annFun $ Action act) >>= (lift . assertCnstr)

  ---------------------
  -- Handle Attributes
  ---------------------

  -- First prepare the explicitly provided attributes
  let (attrIdxList, _) = unzip attrList
  z3AttrIdxList <- mapM getZ3AttrIdx attrIdxList
  attrValList <- sequence $ map (\(x,y) -> mkAttrVal x y) attrList
  let z3AttrList = zip3 (map show attrIdxList) z3AttrIdxList attrValList

  -- Extend the relevant functions
  aim <- use attrInfoMap
  ais <- use attrIdxSort
  mapM (\ (name, attrIdx, val) -> do
      let arr = (fromJust $ aim ^.at name) ^. attrRel
      let avs = (fromJust $ aim ^.at name) ^. attrSort
      newArr <- lift $ mkFreshFuncDecl (name ++ attrRelStr) [actSort, ais] avs
      --
      -- forall_ $ \ (x::Action)  (y::AttrIdx) ->
      --   if (x = act /\ y = attridx) then (newArr x y = attr) else (newArr x y = oldArr)
      --
      let conditional x y = (sameAct x (Action act)) `and_` (Prop $ lift $ mkEq attrIdx $ unAttrVal y)
      let trueBranch (Action x) (AttrVal y) = Prop $ lift $ join $ mkEq <$> (mkApp newArr [x,y]) <*> (return $ unAttrVal val)
      let falseBranch (Action x) (AttrVal y) = Prop $ lift $ join $ mkEq <$> (mkApp newArr [x,y]) <*> (mkApp arr [x,y])
      assertFOL $ forall_ $ \x -> forallAI_ $ \y -> prop $
        ite_ (conditional x y) (trueBranch x y) (falseBranch x y)
      -- extend state
      attrInfoMap.(traverseAt name).attrRel .= newArr
    ) z3AttrList


  return $ Action act
  where
    getZ3Evt evt = do
      exec <- get
      lift $ runReaderT (getEvent evt) exec
    getZ3AttrIdx ais = do
      exec <- get
      lift $ runReaderT (getAttrIdx ais) exec
    assertFOL fol = do
      exec <- get
      lift $ runReaderT (unFOL fol) exec >>= assertCnstr


-- Make a new session.
newSession :: String      -- Session name prefix.
           -> ECD Session -- Return the new session identifier.
newSession sessName = do
  ss <- use sessSort
  -- Allocate a new constant for the new session
  sess <- lift $ mkFreshConst sessName ss

  -- Assert that the new session const is not a previously seen session.
  soup <- use sessSoup
  lift $ mkSetMember sess soup >>= mkNot >>= assertCnstr

  -- Insert the new session constant into the sessSoup
  newSoup <- lift $ mkSetAdd soup sess
  sessSoup .= newSoup
  return $ Session sess


-- Fetch the Z3 Event value corresponding to Haskell Event value.
getEvent :: SolverEvent a => a -> ReaderT Exec Z3 AST
getEvent eventValue = do
  eventSort <- view evtSort
  getCons eventValue eventSort

getAttrIdx :: SolverAttr a => a -> ReaderT Exec Z3 AST
getAttrIdx attrIdxValue = do
  ais <- view attrIdxSort
  getCons attrIdxValue ais

getCons :: Show a => a -> Sort -> ReaderT Exec Z3 AST
getCons value sort = do
  constructors <- lift $ getDatatypeSortConstructors sort
  nameList <- lift $ mapM getDeclName constructors
  strList <- lift $ mapM getSymbolString nameList
  let pList = zip strList constructors
  constructor <- lift $ liftIO . runQ $ do -- Q Monad
    let (Just (_,c)) = find (\ (s,_) -> (show value) == s) pList
    return c
  lift $ mkApp constructor []


-------------------------------------------------------------------------------
-- Consistency checker

assertBasicAxioms :: ReaderT Exec Z3 ()
assertBasicAxioms = do
  -------------
  -- Visibility
  -------------
  orderingAssertions visTo

  ----------------
  -- Session Order
  ----------------
  orderingAssertions sessOrd
  -- Session order only relates actions from the same session
  assertFOL $ forall_ $ \ a -> forall_ $ \b -> prop $
    (a `sessOrd` b) `implies_` (isInSameSess a b)


  -- Actions from the same session are related by session order
  assertFOL $ forall_ $ \a -> forall_ $ \b -> prop $
    ((isInSameSess a b) `and_` (not_ $ sameAct a b)) `implies_`
    ((a `sessOrd` b) `or_` (b `sessOrd` a))

  ---------------------
  -- Strong Consistency
  ---------------------
  ss <- view strgSoup
  assertFOL $ forall_ $ \ a -> forall_ $ \ b -> prop $
    ((Prop $ lift $ mkSetMember (unAct a) ss) `and_`
     (Prop $ lift $ mkSetMember (unAct b) ss) `and_`
     (not_ $ sameAct a b)) `implies_`
    ((a `visTo` b) `or_` (b `visTo` a))

  where
    assertFOL = unFOL >=> (lift . assertCnstr)
    orderingAssertions :: (Action -> Action -> Prop) -- Ordering relation
                       -> ReaderT Exec Z3 ()
    orderingAssertions rel = do
      actSoup <- view actSoup

      -- relation only relates actions in action soup
      assertFOL $ forall_ $ \x -> forall_ $ \y -> prop $
        ((not_ $ Prop $ lift $ mkSetMember (unAct x) actSoup) `or_`
         (not_ $ Prop $ lift $ mkSetMember (unAct y) actSoup))
        `implies_`
        ((not_ $ x `rel` y) `and_` (not_ $ y `rel` x))

      -- relation is irreflexive
      assertFOL $ forall_ $ \x -> prop $ not_ $ x `rel` x

      -- relation is asymmetric
      assertFOL $ forall_ $ \x -> forall_ $ \y -> prop $
        (x `rel` y) `implies_` (not_ $ y `rel` x)

      -- relation is transitive
      assertFOL $ forall_ $ \x -> forall_ $ \y -> forall_ $ \z -> prop $
        ((x `rel` y) `and_` (y `rel` z)) `implies_` (x `rel` z)

checkConsistency :: FOL -> ECD Result
checkConsistency (FOL fol) = do
  exec <- get
  lift $ do
    push
    runReaderT assertBasicAxioms exec
    r <- check
    case r of
      Unsat -> liftIO $ putStrLn "Basic consistency axioms failed"
      otherwise -> return ()
    ast <- runReaderT fol exec
    -- negate the given axiom and check its sat
    mkNot ast >>= assertCnstr
    -- we're looking for Unsat here
    r <- check
    pop 1
    return r

runECD :: EventType -> AttrType -> ECD a -> IO a
runECD evtType attrType ecd = evalZ3 $ do
  exec <- mkExec evtType attrType
  evalStateT ecd exec

doIO :: IO a -> ECD a
doIO = lift . liftIO

liftZ3 :: Z3 a -> ECD a
liftZ3 = lift
