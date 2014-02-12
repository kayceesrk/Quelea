{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, GADTs #-}

module Tracer where

import Language.Haskell.TH
import Z3.Monad
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

newtype EventSort   = EventSort { unEventSort :: Sort }
newtype Action      = Action    { unAct :: AST }
newtype Session     = Session   { unSession :: AST }

data Exec = Exec { -- Soups
                   _actSoup  :: AST,      -- Set Action
                   _sessSoup :: AST,      -- Set Session
                   -- Relations
                   _sessRel  :: FuncDecl, -- Action -> Session
                   _evtRecg  :: FuncDecl, -- Action -> Event -> Bool
                   _visRel   :: FuncDecl, -- Action -> Action -> Bool
                   _soRel    :: FuncDecl, -- Action -> Action -> Bool
                   -- Sorts
                   _actSort  :: Sort,
                   _sessSort :: Sort,
                   _evtSort  :: Sort}

makeLenses ''Exec


-- Make an empty execution.
mkExec :: EventSort -> Z3 Exec
mkExec (EventSort eventSort) = do
  --------
  -- Sorts
  --------
  actionSym <- mkStringSymbol actSortStr
  actionSort <- mkUninterpretedSort actionSym
  sessionSym <- mkStringSymbol sessSortStr
  sessionSort <- mkUninterpretedSort sessionSym
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
  return $ Exec actionSoup sessionSoup      -- Soups
           sessRel evtRecgRel visRel soRel  -- Relations
           actionSort sessionSort eventSort -- Sorts

-- Make a new action.
--
-- Invariants
--  (1) Actions are always added in session order.
--  (2) The session to which the action belongs to is already in the session
--      soup.
--  (3) The visible actions belong to the action soup.
--  (4) The session to which each visible action belongs to belongs to the
--      session soup.
newAction :: Maybe String           -- Action name.
          -> ExpQ                   -- Event.
          -> [Action]               -- Set of visible actions.
          -> Session                -- Session identifier.
          -> StateT Exec Z3 Action  -- Returns the new action.
newAction = undefined -- TODO

-- Make a new session.
newSession :: Maybe String           -- Session name.
           -> StateT Exec Z3 Session -- Return the new session identifier.
newSession maybeStr = do
  ss <- use sessSort
  -- Allocate a new constant for the new session
  sess <- case maybeStr of
    Nothing -> lift $ mkFreshConst "S_" ss
    Just s -> lift $ do
      sessSym <- mkStringSymbol s
      mkConst sessSym ss
  -- Assert that the new session const is not a previously seen session.
  soup <- use sessSoup
  lift $ mkSetMember sess soup >>= mkNot >>= assertCnstr
  -- Insert the new session constant into the sessSoup
  newSoup <- lift $ mkSetAdd soup sess
  sessSoup .= newSoup
  return $ Session sess

-- Create a Z3 Event Sort that mirrors the Haskell Event Type.
createEventSort :: Name               -- ^ Name corresponding to Event type.
                -> IO (Z3 EventSort)
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
  return $ fmap EventSort makeDatatype

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

-- First-order logic.
data FOL where
  Exists_ :: (Action -> FOL) -> FOL
  Forall_ :: (Action -> FOL) -> FOL
  Prop_   :: Prop -> FOL

exists_ = Exists_
forall_ = forall_
prop_   = Prop_

-- Interpreter for first-order logic.
interpFOL :: FOL -> ReaderT Exec Z3 AST
interpFOL (Prop_ p) = unProp p
interpFOL (Forall_ f) = do
  as <- view actSort
  qvConst <- lift $ mkFreshConst "FA_" as
  qv <- lift $ toApp qvConst
  body <- interpFOL $ f (Action qvConst)
  lift $ mkForallConst [] [qv] body
interpFOL (Exists_ f) = do
  as <- view actSort
  qvConst <- lift $ mkFreshConst "EX_" as
  qv <- lift $ toApp qvConst
  body <- interpFOL $ f (Action qvConst)
  lift $ mkExistsConst [] [qv] body

-- Propositional logic type.
newtype Prop = Prop { unProp :: ReaderT Exec Z3 AST }

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
  er <- view evtRecg
  eventAst <- getEvent e
  lift $ mkApp er [a1, eventAst]

isInSameSess :: Action -> Action -> Prop
isInSameSess (Action a1) (Action a2) = Prop $ do
  sr <- view sessRel
  as1 <- lift $ mkApp sr [a1]
  as2 <- lift $ mkApp sr [a2]
  lift $ mkEq a1 a2


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
  assertFOL $ forall_ $ \ a -> forall_ $ \b -> prop_ $
    (a `sessOrd` b) `implies_` (isInSameSess a b)


  -- Actions from the same session are related by session order
  assertFOL $ forall_ $ \a -> forall_ $ \b -> prop_ $
    ((isInSameSess a b) `and_` (not_ $ sameAct a b)) `implies_`
    ((a `sessOrd` b) `or_` (b `sessOrd` b))

  where
    assertFOL = interpFOL >=> (lift . assertCnstr)
    orderingAssertions :: (Action -> Action -> Prop) -- Ordering relation
                       -> ReaderT Exec Z3 ()
    orderingAssertions rel = do
      actSoup <- view actSoup

      -- relation only relates actions in action soup
      assertFOL $ forall_ $ \x -> forall_ $ \y -> prop_ $
        ((not_ $ Prop $ lift $ mkSetMember (unAct x) actSoup) `or_`
         (not_ $ Prop $ lift $ mkSetMember (unAct y) actSoup))
        `implies_`
        ((not_ $ x `rel` y) `and_` (not_ $ y `rel` x))

      -- relation is irreflexive
      assertFOL $ forall_ $ \x -> prop_ $ not_ $ x `rel` x

      -- relation is asymmetric
      assertFOL $ forall_ $ \x -> forall_ $ \y -> prop_ $
        (x `rel` y) `implies_` (not_ $ y `rel` x)

      -- relation is transitive
      assertFOL $ forall_ $ \x -> forall_ $ \y -> forall_ $ \z -> prop_ $
        ((x `rel` y) `and_` (y `rel` z)) `implies_` (x `rel` z)
