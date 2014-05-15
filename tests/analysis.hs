{-# LANGUAGE OverloadedStrings, DataKinds, TypeSynonymInstances, FlexibleInstances, BangPatterns, ScopedTypeVariables #-}

import Spec
import Control.Applicative
import System.Random
import qualified Data.Set as S

trivial :: Spec
trivial x = true

rmw :: Spec
rmw a = forall_ $ \b -> so b a ==> vis b a

tvis :: Spec
tvis z = forall_ $ \x -> forall_ $ \y -> vis x y /\ vis y z ==> vis x z

mw :: Spec
mw x = forall_ $ \a -> forall_ $ \b -> so a b /\ vis b x ==> vis a x

cau :: Spec
cau x = forall_ $ \a -> hb a x ==> vis a x

ex1 spec = do
  s1 <- randomIO
  let x = Addr s1 1
  putStrLn "size(known) = 0"
  isContextReady [] x spec

ex2 spec = do
  s1 <- randomIO
  let x = Addr s1 2
  let y = Row4Z3 s1 1 S.empty
  putStrLn "size(known) = 1"
  isContextReady [y] x spec

ex3 spec = do
  s1 <- randomIO
  let x = Addr s1 2
  putStrLn "size(known) = 0"
  isContextReady [] x spec

ex4 spec = do
  s1 <- randomIO
  s2 <- randomIO
  let y = Row4Z3 s1 1 S.empty
  let x = Addr s1 2
  let z = Row4Z3 s2 1 S.empty
  putStrLn "size(known) = 2"
  isContextReady [y,z] x spec

ex5 spec = do
  s1 <- randomIO
  s2 <- randomIO
  let y = Row4Z3 s1 1 S.empty
  let x = Addr s1 2
  let z = Row4Z3 s2 2 S.empty
  putStrLn "size(known) = 2"
  isContextReady [y,z] x spec

doEx ex str = do
  putStrLn "--------------------------------"
  putStrLn str
  res <- ex
  case res of
    Nothing -> putStrLn ("Not Ready")
    Just s -> putStrLn ("Ready : size(sub-known) = " ++ (show $ length s))

main = do
  doEx (ex1 trivial) "Example 1 + trivial"
  doEx (ex1 rmw)     "Example 1 + rmw"
  doEx (ex1 mw)      "Example 1 + mw"
  doEx (ex1 cau)     "Example 1 + cau"

  doEx (ex2 trivial) "Example 2 + trivial"
  doEx (ex2 rmw)     "Example 2 + rmw"
  doEx (ex2 mw)      "Example 2 + mw"
  doEx (ex2 cau)     "Example 2 + cau"

  doEx (ex3 trivial) "Example 3 + trivial"
  doEx (ex3 rmw)     "Example 3 + rmw"
  doEx (ex3 mw)      "Example 3 + mw"
  doEx (ex3 cau)     "Example 3 + cau"

  doEx (ex4 trivial) "Example 4 + trivial"
  doEx (ex4 rmw)     "Example 4 + rmw"
  doEx (ex4 mw)      "Example 4 + mw"
  doEx (ex4 cau)     "Example 4 + cau"

  doEx (ex5 trivial) "Example 5 + trivial"
  doEx (ex5 rmw)     "Example 5 + rmw"
  doEx (ex5 mw)      "Example 5 + mw"
  doEx (ex5 cau)     "Example 5 + cau"
