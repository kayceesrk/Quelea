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


(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g

newtype EventSort   = EventSort { unEventSort :: Sort }
newtype Axiom       = Axiom     { unAx :: AST }
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
  sessRelSym <- mkStringSymbol sessRelStr
  sessRel <- mkFuncDecl sessRelSym [actionSort] sessionSort

  evtRecgRelSym <- mkStringSymbol evtRecgRelStr
  evtRecgRel <- mkFuncDecl evtRecgRelSym [actionSort, eventSort] boolSort

  visRelSym <- mkStringSymbol visRelStr
  visRel <- mkFuncDecl visRelSym [actionSort, actionSort] boolSort

  soRelSym <- mkStringSymbol soRelStr
  soRel <- mkFuncDecl soRelSym [actionSort, actionSort] boolSort
  ------------
  -- Execution
  ------------
  return $ Exec actionSoup sessionSoup      -- Soups
           sessRel evtRecgRel visRel soRel  -- Relations
           actionSort sessionSort eventSort -- Sorts

-------------------------------------------------------------------------------
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


data FOL where
  Exists :: (Action -> FOL) -> FOL
  Forall :: (Action -> FOL) -> FOL
  Prop   :: Prop -> FOL

data Prop where
  -- Z3AST: Is not exposed in the module.
  Z3AST    :: (Exec -> Z3 AST) -> Prop
  -- Propositional logic constructors
  TrueP    :: Prop
  FalseP   :: Prop
  Not      :: Prop -> Prop
  Or       :: Prop -> Prop -> Prop
  And      :: Prop -> Prop -> Prop
  Implies  :: Prop -> Prop -> Prop
  -- Uninterpreterd functions
  SameAct  :: Action -> Action -> Prop
  VisTo    :: Action -> Action -> Prop
  SessOrd  :: Action -> Action -> Prop
  IsEvent  :: Action -> ExpQ -> Prop


getEvent :: Sort    -- Sort for Event datatype in Z3
         -> ExpQ    -- ExpQ corresponding to Haskell Event value
         -> Z3 AST  -- AST corresponding to Z3 event value
getEvent eventSort eventValue = do -- Z3 Monad
  constructors <- getDatatypeSortConstructors eventSort
  nameList <- mapM getDeclName constructors
  strList <- mapM getSymbolString nameList
  let pList = zip strList constructors
  constructor <- liftIO . runQ $ do -- Q Monad
    ConE name <- eventValue
    let (Just (_,c)) = find (\ (s,_) -> nameBase name == s) pList
    return c
  mkApp constructor []

interpProp :: Prop -> ReaderT Exec Z3 Axiom
interpProp (Z3AST f) = do
  exec <- ask
  lift $ Axiom <$> f exec
interpProp TrueP = lift $ Axiom <$> mkTrue
interpProp FalseP = lift $ Axiom <$> mkFalse
interpProp (Not p) = do
  ast <- unAx <$> interpProp p
  lift $ Axiom <$> mkNot ast
interpProp (Or p1 p2) = do
  ast1 <- unAx <$> interpProp p1
  ast2 <- unAx <$> interpProp p2
  lift $ Axiom <$> mkOr [ast1, ast2]
interpProp (And p1 p2) = do
  ast1 <- unAx <$> interpProp p1
  ast2 <- unAx <$> interpProp p2
  lift $ Axiom <$> mkAnd [ast1, ast2]
interpProp (Implies p1 p2) = do
  ast1 <- unAx <$> interpProp p1
  ast2 <- unAx <$> interpProp p2
  lift $ Axiom <$> mkImplies ast1 ast2
interpProp (SameAct a1 a2) =
  lift $ Axiom <$> mkEq (unAct a1) (unAct a2)
interpProp (VisTo a1 a2) = do
  vr <- view visRel
  lift $ Axiom <$> mkApp vr [unAct a1, unAct a2]
interpProp (SessOrd a1 a2) = do
  so <- view soRel
  lift $ Axiom <$> mkApp so [unAct a1, unAct a2]
interpProp (IsEvent a e) = do
  es <- view evtSort
  eventAst <- lift $ getEvent es e
  er <- view evtRecg
  lift $ Axiom <$> mkApp er [unAct a, eventAst]

interpFOL :: FOL -> ReaderT Exec Z3 Axiom
interpFOL (Prop p) = interpProp p
interpFOL (Exists f) = do
  as <- view actSort
  qvConst <- lift $ mkFreshConst "EX_" as
  qv <- lift $ toApp qvConst
  body <- unAx <$> interpFOL (f (Action qvConst))
  lift $ Axiom <$> mkExistsConst [] [qv] body
interpFOL (Forall f) = do
  as <- view actSort
  qvConst <- lift $ mkFreshConst "FA_" as
  qv <- lift $ toApp qvConst
  body <- unAx <$> interpFOL (f (Action qvConst))
  lift $ Axiom <$> mkForallConst [] [qv] body

createEventSort :: Name -> IO (Z3 EventSort)
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

inSameSess :: Action -> Action -> Prop
inSameSess a b = Z3AST $ \ exec -> do
  a1 <- mkApp (exec^.sessRel) [unAct a]
  a2 <- mkApp (exec^.sessRel) [unAct b]
  mkEq a1 a2

assertBasicAxioms :: ReaderT Exec Z3 ()
assertBasicAxioms = do
  -------------
  -- Visibility
  -------------
  orderingAssertions VisTo

  ----------------
  -- Session Order
  ----------------
  orderingAssertions SessOrd
  -- Session order only relates actions from the same session
  assertFOL $ Forall (\a -> (Forall (\b -> Prop $
    (a `SessOrd` b) `Implies` (inSameSess a b)
    )))

  -- Actions from the same session are related by session order
  assertFOL $ Forall (\a -> (Forall (\b -> Prop $
    ((inSameSess a b) `And` (Not $ SameAct a b)) `Implies`
    ((a `SessOrd` b) `Or` (b `SessOrd` b)))))

  where
    assertFOL = ((fmap unAx) . interpFOL) >=> (lift . assertCnstr)
    --
    orderingAssertions :: (Action -> Action -> Prop) -- Ordering relation
                       -> ReaderT Exec Z3 ()
    orderingAssertions rel = do
      actSoup <- view actSoup
      -- relation only relates actions in action soup
      assertFOL $ Forall (\x -> Forall (\y -> Prop $
        ((Not $ Z3AST $ \ _ -> mkSetMember (unAct x) actSoup) `Or`
        (Not $ Z3AST $ \ _ -> mkSetMember (unAct y) actSoup))
        `Implies`
        ((Not $ x `rel` y) `And`
          (Not $ y `rel` x))))

      -- relation is irreflexive
      assertFOL $ Forall (\x -> Prop $ Not $ x `rel` x)

      -- relation is asymmetric
      assertFOL $ Forall (\x -> Forall (\y -> Prop $
        (x `rel` y) `Implies` (Not $ y `rel` x)))

      -- relation is transitive
      assertFOL $ Forall (\x -> Forall (\y -> Forall (\z -> Prop $
        ((x `rel` y) `And` (y `rel` z)) `Implies`
        (x `rel` z))))
