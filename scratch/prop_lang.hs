{-# LANGUAGE UnicodeSyntax #-}
module Consistency where

data Event = Event String deriving Eq
data SessionId
data ObjectId

data Action

mkEvent :: String -> Event
mkEvent s = Event s

event :: Action -> Event
event = undefined

sess :: Action -> SessionId
sess = undefined

obj :: Action -> ObjectId
obj = undefined

data ActionSoup
data Relation

inRel  :: Relation -> Action -> Action -> Prop
inRel = undefined

inSoup :: ActionSoup -> Action -> Prop
inSoup = undefined

data FOL = Exists (Action -> FOL)
         | Forall (Action -> FOL)
         | Prop Prop

data Prop = Impl Prop Prop
          | Const Bool
          | Not Prop
          | Or Prop Prop
          | And Prop Prop
          | EqAct Action Action
          | EqSess SessionId SessionId
          | EqObj ObjectId ObjectId


foo :: (ActionSoup -> Relation -> FOL)
foo actionSoup visibility =
  Forall $ \x -> Forall $ \y ->
    Prop $ Impl (And (And (inSoup actionSoup x) (inSoup actionSoup y)) (Not (EqAct x y)))
          (Or (inRel visibility x y) (inRel visibility y x))
