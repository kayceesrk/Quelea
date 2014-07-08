{-# LANGUAGE OverloadedStrings, DataKinds, TypeSynonymInstances, FlexibleInstances, BangPatterns, ScopedTypeVariables #-}

import Spec
import Control.Applicative

rmw :: Spec
rmw a = forall_ $ \b -> so b a ==> vis b a

tv :: Spec
tv z = forall_ $ \x -> forall_ $ \y -> vis x y /\ vis y z ==> vis x z

tv2 :: Spec
tv2 x = forall_ $ \y -> forall_ $ \z -> vis x y /\ vis y z ==> vis x z

mw :: Spec
mw x = forall_ $ \a -> forall_ $ \b -> so a b /\ vis b x ==> vis a x

atomicVis :: Spec
atomicVis x = forall_ $ \a -> forall_ $ \b -> (isInSameSess x a) /\ (not_ $ isInSameSess a b) /\ vis a b ==> vis x b

lastEff :: Spec
lastEff x = forall_ $ \a -> vis a x

tv3 :: Spec
tv3 x = forall_ $ \a -> vis a x \/ vis x a \/ sameEffect a x

main = do
  -- show <$> isCoordFree (\_ -> true) >>= putStrLn
  putStrLn "Read-my-writes (RMW) is coordination-free but not available."
  putStr "available(RMW) = "
  show <$> isAvailable rmw >>= putStrLn
  putStr "coordFree(RMW) = "
  show <$> isCoordFree rmw >>= putStrLn

  putStrLn "Trasitive visibility (TV) = forall_ a,b,c. vis a b /\\ vis b c => vis a c is both available and coordination free"
  putStr "available(TV) = "
  show <$> isAvailable tv >>= putStrLn
  putStr "coordFree(TV) = "
  show <$> isCoordFree tv >>= putStrLn

  putStrLn "Tv2. Expect False, False."
  putStr "available(TV2) = "
  show <$> isAvailable tv2 >>= putStrLn
  putStr "coordFree(TV2) = "
  show <$> isCoordFree tv2 >>= putStrLn

  putStrLn "MW is available (and coordination free)"
  putStr "available(MW) = "
  show <$> isAvailable mw >>= putStrLn
  putStr "coordFree(MW) = "
  show <$> isCoordFree mw >>= putStrLn

  show <$> isWellTyped (\x -> true) >>= putStrLn    -- Expect True
  show <$> isWellTyped rmw >>= putStrLn             -- Expect True
  show <$> isWellTyped tv >>= putStrLn              -- Expect True
  show <$> isWellTyped tv2 >>= putStrLn             -- Expect True
  show <$> isWellTyped (\x -> false) >>= putStrLn   -- Expect False
  show <$> isWellTyped (\x -> vis x x) >>= putStrLn -- Expect False
  show <$> isWellTyped (\x -> forall_ $ \y -> so y x ==> vis x y) >>= putStrLn -- Expect False
  show <$> isWellTyped atomicVis >>= putStrLn       -- Expect False
  show <$> isWellTyped lastEff >>= putStrLn         -- Expect False
  show <$> isWellTyped tv3 >>= putStrLn             -- Expect True
