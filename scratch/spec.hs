data LHS
data RHS
data Local
data Global

data Action

data Spec a
data Prop a

forall_ :: (Action -> Spec a) -> Spec a
forall_ f = undefined

(-->) :: Prop LHS -> Prop RHS -> Spec Local
(-->) = undefined

(==>) :: Prop a -> Prop b -> Spec Global
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

rmw :: Spec Local
rmw = forall_ $ \a -> inSo a --> inVis a

mw :: Spec Local
mw = forall_ $ \a -> forall_ $ \b -> so a b /\ inVis b --> inVis a

mr :: Spec Local
mr = forall_ $ \a -> forall_ $ \b -> vis a b /\ inSo b --> inVis a

cvis :: Spec Local
cvis = forall_ $ \a -> forall_ $ \b -> vis a b /\ inVis b --> inVis a

true_ :: Prop a
true_ = undefined

totalOrder :: Spec Global
totalOrder = forall_ $ \a -> forall_ $ \b -> true_ ==> (vis a b \/ vis b a)

main = return ()
