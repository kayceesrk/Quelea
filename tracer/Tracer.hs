{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, GADTs #-}

module Tracer where

import Language.Haskell.TH
import Z3.Monad
import Control.Applicative
import Control.Monad.Reader
import Data.List (find)

newtype EventSort  = EventSort { unEventSort :: Sort }
data Exec = Exec { -- Soups
                   getActSoup  :: AST,      -- Set Action
                   getSessSoup :: AST,      -- Set Session
                   -- Relations
                   getEvtRecg  :: FuncDecl, -- Action -> Event -> Bool
                   getVisRel   :: FuncDecl, -- Action -> Action -> Bool
                   getSoRel    :: FuncDecl, -- Action -> Action -> Bool
                   -- Sorts
                   getActSort   :: Sort,
                   getSessSort  :: Sort,
                   getEventSort :: Sort}
newtype Axiom = Axiom { unAx :: AST }
newtype Action = Action { unAction :: AST }

data FOL where
  Exists :: (Action -> FOL) -> FOL
  Forall :: (Action -> FOL) -> FOL
  Prop   :: Prop -> FOL

data Prop where
  -- Propositional logic constructors
  TrueP    :: Prop
  FalseP   :: Prop
  Not      :: Prop -> Prop
  Or       :: Prop -> Prop -> Prop
  And      :: Prop -> Prop -> Prop
  Impl     :: Prop -> Prop -> Prop
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

interpProp :: Prop -> Exec -> Z3 Axiom
interpProp TrueP _ = Axiom <$> mkTrue
interpProp FalseP _ = Axiom <$> mkFalse
interpProp (Not p) exec = do
  ast <- unAx <$> interpProp p exec
  Axiom <$> mkNot ast
interpProp (Or p1 p2) exec = do
  ast1 <- unAx <$> interpProp p1 exec
  ast2 <- unAx <$> interpProp p2 exec
  Axiom <$> mkOr [ast1, ast2]
interpProp (And p1 p2) exec = do
  ast1 <- unAx <$> interpProp p1 exec
  ast2 <- unAx <$> interpProp p2 exec
  Axiom <$> mkAnd [ast1, ast2]
interpProp (Impl p1 p2) exec = do
  ast1 <- unAx <$> interpProp p1 exec
  ast2 <- unAx <$> interpProp p2 exec
  Axiom <$> mkImplies ast1 ast2
interpProp (SameAct a1 a2) exec =
  Axiom <$> mkEq (unAction a1) (unAction a2)
interpProp (VisTo a1 a2) exec =
  Axiom <$> mkApp (getVisRel exec) [unAction a1, unAction a2]
interpProp (SessOrd a1 a2) exec =
  Axiom <$> mkApp (getSoRel exec) [unAction a1, unAction a2]
interpProp (IsEvent a e) exec = do
  {- eventAst has type Event in Z3 -}
  eventAst <- getEvent (getEventSort exec) e
  Axiom <$> mkApp (getEvtRecg exec) [unAction a, eventAst]


interpFOL :: FOL -> Exec -> Z3 Axiom
interpFOL (Prop p) exec = interpProp p exec
interpFOL (Exists f) exec = do
  qvConst <- mkFreshConst "EX_" $ getActSort exec
  qv <- toApp qvConst
  body <- unAx <$> interpFOL (f (Action qvConst)) exec
  Axiom <$> mkExistsConst [] [qv] body
interpFOL (Forall f) exec = do
  qvConst <- mkFreshConst "FA_" $ getActSort exec
  qv <- toApp qvConst
  body <- unAx <$> interpFOL (f (Action qvConst)) exec
  Axiom <$> mkForallConst [] [qv] body

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
