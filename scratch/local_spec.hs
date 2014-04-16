data LHS
data RHS
data Action
data FOL

data Prop a

forall_ :: (Action -> FOL) -> FOL
forall_ f = undefined

(==>) :: Prop LHS -> Prop RHS -> FOL
(==>) = undefined

inVis :: Action -> Prop a
inVis = undefined

inSo :: Action -> Prop LHS
inSo = undefined

vis :: Action -> Action -> Prop LHS
vis = undefined

so :: Action -> Action -> Prop LHS
so = undefined

(\/) :: Prop a -> Prop a -> Prop a
(\/) = undefined

(/\) :: Prop a -> Prop a -> Prop a
(/\) = undefined

rmw :: FOL
rmw = forall_ $ \a -> inSo a ==> inVis a

mw :: FOL
mw = forall_ $ \a -> forall_ $ \b -> so a b /\ inVis b ==> inVis a

mr :: FOL
mr = forall_ $ \a -> forall_ $ \b -> vis a b /\ inSo b ==> inVis a

cvis :: FOL
cvis = forall_ $ \a -> forall_ $ \b -> vis a b /\ inVis b ==> inVis a

main = return ()
