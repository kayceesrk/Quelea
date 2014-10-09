{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Codeec.Contract.Language (
  Rel(..),
  Prop(..),
  Fol(..),
  Contract,
  Effect(..),
  Z3CtrtState(..),
  Z3Ctrt(..),

  true,
  vis,
  so,
  soo,
  hb,
  sameEff,
  sameTxn,
  sameObj,
  sameObjList,
  (∩),
  (∪),
  (∧),
  (∨),
  (⇒),
  (^+),
  appRel,
  liftProp,
  forall_,
  forall2_,
  forall3_,
  forall4_,
  forallQ_,
  forallQ2_,
  forallQ3_,
  forallQ4_,

  trans
) where


import Codeec.Types
import Z3.Monad
import qualified Data.Map as M
import Control.Monad.State

-------------------------------------------------------------------------------
-- Types

data Z3CtrtState = Z3CtrtState {
  -- Sorts
  _effSort  :: Sort,
  _operSort :: Sort,
  -- Relations
  _visRel     :: FuncDecl, -- Effect -> Effect -> Bool
  _soRel      :: FuncDecl, -- Effect -> Effect -> Bool
  _sameobjRel :: FuncDecl, -- Effect -> Effect -> Bool
  _sameTxnRel :: FuncDecl, -- Effect -> Effect -> Bool
  _curTxnRel  :: FuncDecl, -- Effect -> Bool
  _operRel    :: FuncDecl, -- Effect -> Oper
  -- Map
  _effMap :: M.Map Effect AST,
  -- Assertions
  _assertions :: [AST],
  -- TC Rels
  _tcRelMap :: M.Map Rel FuncDecl
}

data Z3Ctrt = Z3Ctrt { unZ3Ctrt :: StateT Z3CtrtState Z3 AST }

newtype Effect = Effect { unEffect :: Int } deriving (Eq, Ord)

data Rel = Vis | So | SameObj | TC Rel | SameEff | SameTxn
         | Union Rel Rel | Intersect Rel Rel deriving Ord

instance Eq Rel where
  rel1 == rel2 =
    case (rel1, rel2) of
      (Vis, Vis) -> True
      (So, So) -> True
      (SameObj, SameObj) -> True
      (SameTxn, SameTxn) -> True
      (TC r1, TC r2) -> r1 == r2
      (SameEff, SameEff) -> True
      (Union r1 r2, Union r3 r4) ->
        (r1 == r3 && r2 == r4) || (r2 == r3 && r1 == r4)
      (Intersect r1 r2, Intersect r3 r4) ->
        (r1 == r3 && r2 == r4) || (r2 == r3 && r1 == r4)
      otherwise -> False


data OperationClass a => Prop a =
    PTrue | Not (Prop a) | AppRel Rel Effect Effect | Conj (Prop a) (Prop a) | CurTxn Effect
  | Disj (Prop a) (Prop a) | Impl (Prop a) (Prop a) | Oper Effect a | Raw Z3Ctrt
data OperationClass a => Fol a = Forall [a] (Effect -> Fol a) | Plain (Prop a)

type Contract a = Effect -> Fol a

-------------------------------------------------------------------------------
-- Contract builder

true :: Prop a
true = PTrue

vis :: Effect -> Effect -> Prop a
vis a b = AppRel Vis a b

so :: Effect -> Effect -> Prop a
so a b = AppRel So a b

soo :: Effect -> Effect -> Prop a
soo a b = AppRel (So ∩ SameObj) a b

hb :: Effect -> Effect -> Prop a
hb a b = AppRel (TC $ Union Vis So) a b

sameTxn :: Effect -> Effect -> Prop a
sameTxn = AppRel SameTxn

sameObj :: Effect -> Effect -> Prop a
sameObj a b = AppRel SameObj a b

sameObjList :: OperationClass a => [Effect] -> Prop a
sameObjList [] = PTrue
sameObjList (x:[]) = PTrue
sameObjList (x:y:tail) =
  let p = AppRel SameObj x y
  in p ∧ (sameObjList $ y:tail)

(∪) :: Rel -> Rel -> Rel
a ∪ b = Union a b

(∩) :: Rel -> Rel -> Rel
a ∩ b = Intersect a b

(∧) :: OperationClass a => Prop a -> Prop a -> Prop a
(∧) = Conj

(∨) :: OperationClass a => Prop a -> Prop a -> Prop a
(∨) = Disj

(⇒) :: OperationClass a => Prop a -> Prop a -> Prop a
(⇒) = Impl

(^+) :: Rel -> Rel
(^+) = TC

appRel :: OperationClass a => Rel -> Effect -> Effect -> Prop a
appRel = AppRel

liftProp :: OperationClass a => Prop a -> Fol a
liftProp = Plain

forall_ :: OperationClass a => (Effect -> Fol a) -> Fol a
forall_ f = Forall [] f

forall2_ :: OperationClass a => (Effect -> Effect -> Fol a) -> Fol a
forall2_ f = forall_ $ \a -> forall_ $ \b -> f a b

forall3_ :: OperationClass a => (Effect -> Effect -> Effect -> Fol a) -> Fol a
forall3_ f = forall_ $ \a -> forall_ $ \b -> forall_ $ \c -> f a b c

forall4_ :: OperationClass a => (Effect -> Effect -> Effect -> Effect -> Fol a) -> Fol a
forall4_ f = forall_ $ \a -> forall_ $ \b -> forall_ $ \c -> forall_ $ \d -> f a b c d

forallQ_ :: OperationClass a => [a] -> (Effect -> Fol a) -> Fol a
forallQ_ q f = Forall q f

forallQ2_ :: OperationClass a => [a] -> [a] -> (Effect -> Effect -> Fol a) -> Fol a
forallQ2_ l1 l2 f = forallQ_ l1 $ \a -> forallQ_ l2 $ \b -> f a b

forallQ3_ :: OperationClass a => [a] -> [a] -> [a] -> (Effect -> Effect -> Effect -> Fol a) -> Fol a
forallQ3_ l1 l2 l3 f = forallQ_ l1 $ \a -> forallQ_ l2 $ \b -> forallQ_ l3 $ \c -> f a b c

forallQ4_ :: OperationClass a => [a] -> [a] -> [a] -> [a] -> (Effect -> Effect -> Effect -> Effect -> Fol a) -> Fol a
forallQ4_ l1 l2 l3 l4 f = forallQ_ l1 $ \a -> forallQ_ l2 $ \b -> forallQ_ l3 $ \c -> forallQ_ l4 $ \d -> f a b c d


sameEff :: Effect -> Effect -> Prop a
sameEff a b = AppRel SameEff a b

-- Given trans [a,b][c,d], translate it to (a ~ b) ∧ (c ~ d) ∧ (not $ a ~ c).
-- The rest follows.
trans :: OperationClass a => [[Effect]] -> Prop a
trans lol =
  let stRel = foldl (\acc el -> acc ∧ (mkRelTxn (AppRel SameTxn) el)) true lol
      pick1List = foldl (\acc el -> case el of {x:xs -> x:acc; [] -> acc}) [] lol
      dtRel = mkRelTxn (\x y -> Not $ AppRel SameTxn x y) pick1List
  in (mkCurTxn lol) ∧ stRel ∧ dtRel
  where
    mkCurTxn (xl:xsl) =
      let e:_ = xl
      in CurTxn e
    mkRelTxn rel [] = PTrue
    mkRelTxn rel (x:xs) =
      let stList = map (\y -> rel x y) xs
      in foldl (\acc r -> acc ∧ r) true stList
