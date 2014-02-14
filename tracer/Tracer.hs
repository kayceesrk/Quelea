{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, GADTs, DeriveFunctor #-}

module Tracer
( Action,
  Session,
  ECD,
  Result (..),

  FOL,
  forall_,
  exists_,
  prop,

  Prop,
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

  newAction,
  newSession,
  checkConsistency,
  runECD,
  doIO
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

-------------------------------------------------------------------------------
-- Helper functions

-- Monadic composition
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g

-------------------------------------------------------------------------------
-- Types

type ECD a = StateT Exec Z3 a
type Result = Z3M.Result

newtype EventSort   = EventSort { unEventSort :: Sort }
newtype Action      = Action    { unAct :: AST }
newtype Event       = Event     { unEvent :: AST }
newtype Session     = Session   { unSession :: AST }

data Exec = Exec { -- Soups
                   _actSoup  :: AST,      -- Set Action
                   _sessSoup :: AST,      -- Set Session
                   -- Relations
                   _sessRel     :: FuncDecl, -- Action -> Session
                   _evtRecgRel  :: FuncDecl, -- Action -> Event -> Bool
                   _visRel      :: FuncDecl, -- Action -> Action -> Bool
                   _soRel       :: FuncDecl, -- Action -> Action -> Bool
                   -- Sorts
                   _actSort  :: Sort,
                   _sessSort :: Sort,
                   _evtSort  :: Sort}

-- First-order logic.
newtype FOL = FOL { unFOL :: ReaderT Exec Z3 AST }

-- Propositional logic type.
newtype Prop = Prop { unProp :: ReaderT Exec Z3 AST }

makeLenses ''Exec

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
  ast1 <- p2
  ast2 <- p2
  lift $ mkAnd [ast1, ast2]

implies_ :: Prop -> Prop -> Prop
implies_ (Prop p1) (Prop p2) = Prop $ do
  ast1 <- p2
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

isEvent :: Action -> ExpQ -> Prop
isEvent (Action a1) e = Prop $ do
  er <- view evtRecgRel
  eventAst <- getEvent e
  lift $ mkApp er [a1, eventAst]

isInSameSess :: Action -> Action -> Prop
isInSameSess (Action a1) (Action a2) = Prop $ do
  sr <- view sessRel
  as1 <- lift $ mkApp sr [a1]
  as2 <- lift $ mkApp sr [a2]
  lift $ mkEq a1 a2

ite_ :: Prop -> Prop -> Prop -> Prop
ite_ (Prop p1) (Prop p2) (Prop p3) = Prop $ do
  ast1 <- p1
  ast2 <- p2
  ast3 <- p3
  lift $ mkIte ast1 ast2 ast3

-------------------------------------------------------------------------------
-- Execution builder

-- Create a Z3 Event Sort that mirrors the Haskell Event Type.
createEventSort :: Name         -- ^ Name corresponding to Event type.
                -> IO (Z3 Sort)
createEventSort t = do
  TyConI (DataD _ (typeName::Name) _ constructors _) <- runQ $ reify t
  let empty :: Z3 [Constructor] = return []
  let makeCons consStr = do
      consSym <- mkStringSymbol consStr
      isConsSym <- mkStringSymbol $ "is_" ++ consStr
      mkConstructor consSym isConsSym []
  let bar (acc :: Z3 [Constructor]) (NormalC name _) = do
      consList <- acc
      newCons <- makeCons $ nameBase name
      return (newCons:consList)
  let makeDatatype = do
      consList <- foldl bar empty constructors
      dtSym <- mkStringSymbol (nameBase typeName)
      mkDatatype dtSym consList
  return makeDatatype

-- Make an empty execution.
mkExec :: Name -> Z3 Exec
mkExec evtName = do
  --------
  -- Sorts
  --------
  actionSym <- mkStringSymbol actSortStr
  actionSort <- mkUninterpretedSort actionSym

  sessionSym <- mkStringSymbol sessSortStr
  sessionSort <- mkUninterpretedSort sessionSym

  mkEventSort <- liftIO $ createEventSort evtName
  eventSort <- mkEventSort
  boolSort <- mkBoolSort
  --------
  -- Soups
  --------
  actionSoup <- mkEmptySet actionSort
  sessionSoup <- mkEmptySet sessionSort
  ------------
  -- Relations
  ------------
  sessRel <- mkFreshFuncDecl sessRelStr [actionSort] sessionSort

  evtRecgRel <- mkFreshFuncDecl evtRecgRelStr [actionSort, eventSort] boolSort

  visRel <- mkFreshFuncDecl visRelStr [actionSort, actionSort] boolSort

  soRel <- mkFreshFuncDecl soRelStr [actionSort, actionSort] boolSort
  ------------
  -- Execution
  ------------
  let exec = Exec actionSoup sessionSoup      -- Soups
             sessRel evtRecgRel visRel soRel  -- Relations
             actionSort sessionSort eventSort -- Sorts

  -- Assertion to create an empty event recognition relation
  -- ∀x.∀y. evtRecgRel x y = false
  assertFOL exec $ forall_ $ \ (Action x) -> forallE_ $ \ (Event y) ->
                    prop $ Prop $ lift $ mkApp evtRecgRel [x,y] >>= mkNot

  return exec
  where
    assertFOL exec (FOL fol) = do
      ast <- runReaderT fol exec
      assertCnstr ast

-- Make a new action.
--
-- Invariants
--  (1) Actions are always added in session order.
--  (2) The session to which the action belongs to is already in the session
--      soup.
newAction :: String                 -- Action name prefix.
          -> ExpQ                   -- Event.
          -> (Action -> FOL)        -- Consistency annotation
          -> Session                -- Session identifier.
          -> ECD Action             -- Returns the new action.
newAction actStr evt annFun sess = do
  actSort <- use actSort
  sessSort <- use sessSort
  evtSort <- use evtSort

  -- make a new action
  act <- lift $ mkFreshConst actStr actSort

  -- Extend action soup
  as <- use actSoup
  lift $ mkSetMember act as >>= mkNot >>= assertCnstr
  newAs <- lift $ mkSetAdd as act
  actSoup .= newAs

  -- Extend sessRel
  sr <- use sessRel
  newSr <- lift $ mkFreshFuncDecl sessRelStr [actSort] sessSort
  -- ∀x. if (x = act) then (newSr act = sess) else (newSr x = sr x)
  let trueBranch x = Prop $ lift $ mkApp newSr [unAct x] >>= mkEq (unSession sess)
  let falseBranch x = Prop $ lift $ join $ mkEq <$> (mkApp newSr [unAct x]) <*> (mkApp sr [unAct x])
  assertFOL $ forall_ $ \x -> prop $ ite_ (sameAct x (Action act)) (trueBranch x) (falseBranch x)
  sessRel .= newSr

  -- Extend soRel
  sor <- use soRel
  newSor <- lift $ mkFreshFuncDecl soRelStr [actSort, actSort] =<< mkBoolSort
  -- ∀x. sr(x) = sess ⇒ x ――so―→ act
  -- Other axioms, such as so is total order, so only relates actions in the
  -- same session are available in basic consistency axioms.
  let antecedent x = Prop $ lift $ mkEq (unSession sess) =<< mkApp sr [unAct x]
  let consequent x = x `sessOrd` (Action act)
  assertFOL $ forall_ $ \x -> prop $ (antecedent x) `implies_` (consequent x)
  soRel .= newSor

  -- Extend evtRecgRel
  err <- use evtRecgRel
  newErr <- lift $ mkFreshFuncDecl evtRecgRelStr [actSort, evtSort] =<< mkBoolSort
  z3Evt <- getZ3Evt evt
  -- ∀x:Action.∀y:Event. if (x = act) then (newErr x evt) else (newErr x y = err x y)
  let conditional x = sameAct x (Action act)
  let trueBranch x = Prop $ lift $ mkApp newErr [unAct x, z3Evt]
  let falseBranch x y = Prop $ lift $ join $
        mkEq <$> (mkApp newErr [unAct x, unEvent y]) <*> (mkApp err [unAct x, unEvent y])
  assertFOL $ forall_ $ \x -> forallE_ $ \y -> prop $ ite_ (conditional x) (trueBranch x) (falseBranch x y)
  evtRecgRel .= newErr

  -- Assert the consistency annotation
  assertFOL $ annFun $ Action act

  return $ Action act
  where
    getZ3Evt evt = do
      exec <- get
      lift $ runReaderT (getEvent evt) exec
    assertFOL fol = do
      exec <- get
      lift $ runReaderT (unFOL fol) exec >>= assertCnstr


-- Make a new session.
newSession :: String                 -- Session name prefix.
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
getEvent :: ExpQ                  -- ^ Expression corresponding to Event type.
         -> ReaderT Exec Z3 AST
getEvent eventValue = do
  eventSort <- view evtSort
  constructors <- lift $ getDatatypeSortConstructors eventSort
  nameList <- lift $ mapM getDeclName constructors
  strList <- lift $ mapM getSymbolString nameList
  let pList = zip strList constructors
  constructor <- lift $ liftIO . runQ $ do -- Q Monad
    ConE name <- eventValue
    let (Just (_,c)) = find (\ (s,_) -> nameBase name == s) pList
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
    ((a `sessOrd` b) `or_` (b `sessOrd` b))

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
    ast <- runReaderT fol exec
    assertCnstr ast
    r <- check
    pop 1
    return r

runECD :: Name -> ECD a -> IO a
runECD evtName ecd = evalZ3 $ do
  exec <- mkExec evtName
  evalStateT ecd exec

doIO :: IO a -> ECD a
doIO = lift . liftIO
