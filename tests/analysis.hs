{-# LANGUAGE OverloadedStrings, DataKinds, TypeSynonymInstances, FlexibleInstances, BangPatterns, ScopedTypeVariables #-}

import Spec
import Control.Applicative

rmw :: Spec
rmw a = forall_ $ \b -> so b a ==> vis b a

tv :: Spec
tv z = forall_ $ \x -> forall_ $ \y -> vis x y /\ vis y z ==> vis x z

tv2 :: Spec
tv2 x = forall_ $ \y -> forall_ $ \z -> vis x y /\ vis y z ==> vis x z

main = do
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
  show <$> isAvailable tv >>= putStrLn
  putStr "coordFree(TV2) = "
  show <$> isCoordFree tv >>= putStrLn

