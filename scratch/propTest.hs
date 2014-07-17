import Codeec.Contract hiding (liftProp)
import qualified Codeec.Contract as C (liftProp)

liftProp :: Prop () -> Fol ()
liftProp = C.liftProp

tvis :: Fol ()
tvis = forall_ $ \a -> forall_ $ \b -> liftProp $ appRel ((^+) Vis) a b ⇒ vis a b

test :: Fol ()
test = forall_ $ \a -> forall_ $ \b -> forall_ $ \c -> liftProp $ (vis a b ∧ vis b c) ⇒ vis a c

main = do
  r <- isValid "true" $ liftProp $ (Raw $ fol2Z3Ctrt tvis) ⇒ (Raw $ fol2Z3Ctrt test)
  putStrLn $ show r
